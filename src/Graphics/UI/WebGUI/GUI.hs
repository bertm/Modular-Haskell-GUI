{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

-- | The actual GUI implementation, without the server part.
-- This module defines the way the GUI is presented to the programmer, as well as some
-- GUI logic to keep track of the state.

module Graphics.UI.WebGUI.GUI (
        -- * Server binding
        processor,
        connections,
        InputToken,
        OutputToken,
        
        -- * Widgets
        module Graphics.UI.WebGUI.Widgets,
        module Graphics.UI.WebGUI.Properties,
        module Graphics.UI.WebGUI.Events,
        Screen,
        
        -- * Interaction
        onEvent,
        quit
    ) where

import Control.Concurrent.MVar
import Control.Concurrent
import Data.HashTable (HashTable)
import qualified Data.HashTable as HT
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Maybe
import System.Timer.Updatable

import Graphics.UI.WebGUI.Widgets hiding (Screen)
import Graphics.UI.WebGUI.Widgets.Internal
import Graphics.UI.WebGUI.Widgets.Properties
import Graphics.UI.WebGUI.Widgets.Protocol
import qualified Graphics.UI.WebGUI.Widgets as W

import Graphics.UI.WebGUI.Properties
import Graphics.UI.WebGUI.Properties.Internal
import Graphics.UI.WebGUI.Properties.Protocol
import Graphics.UI.WebGUI.Properties.Props

import Graphics.UI.WebGUI.Events
import Graphics.UI.WebGUI.Events.Protocol
import Graphics.UI.WebGUI.Types
import Graphics.UI.WebGUI.Buffer
import Graphics.UI.WebGUI.Tokens
import Graphics.UI.WebGUI.Actions

-- | GUI version.
version :: Version
version = "1.0"

type Screen = W.Screen () Object
type Type = String
type EventHandler = Event -> IO ()

-- | Holds information on the state of a GUI.
data Global = Global { out :: Buffer OutputToken
                     , nextId :: MVar Identifier
                     , props :: State [Prop]
                     , events :: State [(Event, EventHandler)]
                     , children :: State [Identifier]
                     , types :: State String
                     , closed :: TVar Bool
                     , resetTimeout :: IO ()
                     }

-- | A wrapper that allows passing Widgets freely.
data Object = Object Type Identifier Global

-- | Holds the state in the IO monad.
type State a = HashTable Identifier a

-- | Creates an empty State.
newState :: IO (State a)
newState = HT.fromList (HT.hashInt . fromInteger) []

-- | Lookup the value for the given Identifier in the State.
getState :: State a -> Identifier -> IO (Maybe a)
getState state i = HT.lookup state i

-- | Set the value for the given Identifier in the State.
-- Passing Nothing as value removes the Identifier from the State.
setState :: State a -> Identifier -> Maybe a -> IO ()
setState state i (Just s) = HT.update state i s >>= const (return ())
setState state i Nothing  = HT.delete state i

-- | Keeps track of connections.
type Connections = State (Buffer InputToken, Buffer OutputToken)
connections :: IO Connections
connections = newState

-- | Handles the incoming connection and starts the GUI.
processor :: (Screen -> IO ()) -> Connections -> Delay -> Buffer InputToken -> Buffer OutputToken -> IO ()
processor main cons timeout inp out =
  do -- Check if first token is Establish with the correct version.
     first <- bGetIO inp
     case first of
       IEstablish v -> if v == version
                         then -- We're good, (re)start the GUI.
                              maybeRestart first
                         else serverError out "Wrong client version"
       _            -> serverError out "Handshake failed"
  where maybeRestart f = do connId <- return 1            -- TODO: make unique connection ID using
                            b <- getState cons connId     -- persistent client storage,
                            case Nothing of {-case b of-} -- and then restore this.
                              Just (i, o) -> atomically $ do bUnGet inp f
                                                             bind i inp
                                                             bind o out
                              Nothing -> do setState cons connId $ Just (inp, out)
                                            bPutIO out $ OAcknowledge version
                                            run
        run = do -- Initialize states.
                 props <- newState
                 events <- newState
                 nextId <- newMVar 1000
                 children <- newState
                 setState children 0 $ Just []
                 types <- newState
                 closed <- newTVarIO False
                 let delay = (timeout + 60) * 10^6
                 keepalive <- replacer (atomically $ writeTVar closed True) delay
                 let resetTimeout = renewIO keepalive delay
                 global <- return $ Global out nextId props events children types closed resetTimeout
                 conn <- getScreen global
                 -- Fire the GUI thread with the application logic.
                 actionThread <- forkIO $ main conn
                 -- Process incoming tokens.
                 processor' global `finally` killThread actionThread
        processor' :: Global -> IO ()
        processor' global = do serverInp <- atomically
                                $ do i <- bTryGet inp
                                     s <- readTVar $ closed global
                                     case (i, s) of
                                       (_, True) -> return Nothing
                                       (Just r, False) -> return (Just r)
                                       _ -> retry
                               case serverInp of
                                 Just i  -> do handleServer global out i
                                               processor' global
                                 Nothing -> do bPutIO out OClose
                                               return ()

-- | Handles an incoming token.
handleServer :: Global -> Buffer OutputToken -> InputToken -> IO ()
handleServer global out token =
  case token of
    IEstablish v -> do -- Restore the session state.
                       bPutIO out $ OAcknowledge version
                       objs <- HT.toList $ types global
                       mapM_ (\(i, t) -> oCreate global i t) objs
                       prps <- HT.toList $ props global
                       mapM_ (\(i, ps) -> mapM_ (oSet global i) ps) prps
                       chls <- HT.toList $ children global
                       mapM_ (\(p, cs) -> mapM_ (\c -> oAction global p "add" [ObjectV [("id", IntegerV c)]]) $ reverse cs) chls
    ISignal id name time args -> do print $ toEvent name-- TODO: implement properly
                                    es <- getHandlerState (events global) id (toEvent name)
                                    mapM_ (\x -> x (toEvent name)) es
    IKeepalive -> resetTimeout global
    IClose -> getScreen global >>= quit
    IError msg -> do serverError out ("Client error: " ++ msg) -- TODO: Shouldn't we just close the connection, without OError?
                     getScreen global >>= quit
    ISet id name value -> case protoToProp (name, value) of
                            Just prop -> do putPropertyState True (props global) id prop
                                            es <- getHandlerState (events global) id (ChangeEvent prop)
                                            mapM_ (\x -> x undefined) es
                            Nothing -> putStrLn $ "Client tried to set unknown property '" ++ name ++ "' on object no. " ++ show id ++ " to: " ++ show value
    IUnknown    -> serverError out "Received unrecognized token"

-- | Convenience function for reporting a server error.
serverError :: Buffer OutputToken -> String -> IO ()
serverError out s = do putStrLn ("ERROR: " ++ s)
                       bPutIO out $ OError s

-- | Closes the connection, thereby stopping this application.
quit :: Screen -> IO ()
quit s = let (Object _ _ g) = guiObject s in atomically $ writeTVar (closed g) True

-- | Convenience function for outputting a Create token.
oCreate :: Global -> Identifier -> String -> IO ()
oCreate g i t = bPutIO (out g) $ OCreate i t

-- | Convenience function for outputting a Set token.
oSet :: Global -> Identifier -> Prop -> IO ()
oSet g i prop = case propToProto prop of
                  Just (p, v) -> bPutIO (out g) $ OSet i p v
                  _           -> return ()

-- | Convenience function for outputting a Action token.
oAction :: Global -> Identifier -> String -> [Value] -> IO ()
oAction g i n a = bPutIO (out g) $ OAction i n a

-- | Adds or renews a property to the state of the given Identifier.
-- The first argument defines whether only renewal is allowed.
putPropertyState :: Bool -> State [Prop] -> Identifier -> Prop -> IO ()
putPropertyState safe s i p = do mps <- getState s i
                                 case mps of
                                   Just ps -> case span (not . sameProp p) ps of
                                               (a, []) -> if safe
                                                           then return ()
                                                           else setState s i $ Just (p:a)
                                               (a, (old:b)) -> setState s i $ Just ((mergeProp old p) : a ++ b)
                                   Nothing -> if safe
                                               then return ()
                                               else setState s i $ Just [p]

-- | Returns the current value of the Prop in the state of the given Identifier.
getPropertyState :: State [Prop] -> Identifier -> Prop -> IO Prop
getPropertyState s i p = do mps <- getState s i
                            case mps of
                                   Just ps -> case span (not . sameProp p) ps of
                                               (a, []) -> error "Property error: no such property"
                                               (a, (x:b)) -> return x
                                   Nothing -> error "Property error: no such object"

-- | Adds a handler to the event handlers of the given Idenfitier.
putHandlerState :: State [(Event, EventHandler)] -> Identifier -> (Event, EventHandler) -> IO ()
putHandlerState s i e = do mes <- getState s i
                           case mes of
                             Just es -> setState s i $ Just (e:es)
                             Nothing -> setState s i $ Just [e]

-- | Gets the handlers of the given Identifier.
getHandlerState :: State [(Event, EventHandler)] -> Identifier -> Event -> IO [EventHandler]
getHandlerState s i e = do mes <- getState s i
                           case mes of
                                  Just es -> return $ map snd . filter (\x -> fst x == e) $ es
                                  Nothing -> return []

instance PropertyObject Object where
    unsafeSet (Object t i g) prop = do putPropertyState False (props g) i (toProp prop)
                                       oSet g i (toProp prop)
    unsafeGet (Object t i g) prop = getPropertyState (props g) i (toProp (prop $ error "Property unsafeGet error: undefined"))
    unsafeOnChange (Object t i g) prop f = putHandlerState (events g) i (ChangeEvent (toProp $ prop $ error "Property onChange error: undefined"), const f)

instance EventObject Object Event where
    onEvent (Object t i g) e f = putHandlerState (events g) i (e, f)

instance ActionAddRemove Object Object where
    add (Object t p g) cc@(Object _ c _) = do ParentProp (Parent n) <- unsafeGet cc Parent
                                              case n of
                                                Just _ -> error "Widget add error: already added to another widget"
                                                Nothing -> return ()
                                              s <- getState (children g) p
                                              case s of
                                                Just cs -> setState (children g) p $ Just (c:cs)
                                                Nothing -> setState (children g) p $ Just [c]
                                              if p >= 1000 -- TODO: should we remove this distinction?
                                                then oAction g p "add" [ObjectV [("id", IntegerV c)]]
                                                else return ()
                                              unsafeSet cc (Parent $ Just p)
    remove (Object t p g) cc@(Object _ c _) = do ParentProp (Parent n) <- unsafeGet cc Parent
                                                 case n of
                                                   Just i -> if i == p
                                                               then return ()
                                                               else error "Widget remove error: removing from wrong parent"
                                                   Nothing -> error "Widget remove error: removing orphan"
                                                 s <- getState (children g) p
                                                 case s of
                                                   Just cs -> setState (children g) p $ Just (filter (/= c) cs)
                                                   Nothing -> error "Widget remove error: inconsistent state"
                                                 if p >= 1000 -- TODO: should we remove this distinction?
                                                   then oAction g p "remove" [ObjectV [("id", IntegerV c)]]
                                                   else return ()
                                                 unsafeSet cc (Parent Nothing)

instance ActionEvents Object where
    enableEvents (Object t p g) e = oAction g p "enableEvents" [IntegerV $ sum $ map eventBitmask e]
    disableEvents (Object t p g) e = oAction g p "disableEvents" [IntegerV $ sum $ map eventBitmask e]

instance NewWidget Object (ObjectT a Object) where
  new ds p = let Object _ _ g = p
             in do i <- getNextId g
                   let o = widgetObject $ Object t i g
                       t = widgetClass o
                    in do setState (types g) i (Just t)
                          oCreate g i t
                          set o ds -- Perform all initialization.
                          return o

-- | Gets the next unique Identifier.
getNextId :: Global -> IO Identifier
getNextId global = let m = nextId global
                   in do i <- takeMVar m
                         putMVar m (i + 1)
                         return i

-- | Returns the screen that represents the given Global.
getScreen :: Global -> IO Screen
getScreen global = return $ widgetObject (Object "Screen" 2 global)

