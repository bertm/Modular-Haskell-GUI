{-# OPTIONS_GHC -XMultiParamTypeClasses -XFlexibleContexts -XFlexibleInstances #-}

-- | The actual GUI implementation, without the server part.
-- This module defines the way the GUI is presented to the programmer, as well as some
-- GUI logic to keep track of the state.

module GUI (
        -- * Server binding
        processor,
        connections,
        InputToken,
        OutputToken,
        
        -- * Widgets
        module Widgets,
        module Properties,
        module Events,
        Connection,
        
        -- * Interaction
        on
    ) where

import Control.Concurrent.MVar
import Control.Concurrent
import Data.HashTable (HashTable)
import qualified Data.HashTable as HT
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Maybe

import Widgets hiding (Screen)
import Widgets.Internal
import Widgets.Widgets
import Widgets.Properties
import Widgets.Protocol
import Widgets.New

import Properties
import Properties.Internal
import Properties.Protocol
import Properties.Props

import Events
import Events.Protocol
import Types
import Buffer
import Tokens
import Actions

-- | GUI version.
version :: Version
version = "1.0"

type Parent = Maybe Identifier
type Connection = Screen () Object
type Type = String
type EventHandler = Event -> IO ()

-- | Holds information on the state of a GUI.
data Global = Global { out :: Buffer OutputToken
                     , nextId :: MVar Identifier
                     , props :: State [Prop]
                     , events :: State [(Event, EventHandler)]
                     , children :: State [Identifier]
                     , types :: State String
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
processor :: (Connection -> IO ()) -> Connections -> Buffer InputToken -> Buffer OutputToken -> IO ()
processor main cons inp out = do -- Check if first token is Establish with the correct version.
                                 first <- bGetIO inp
                                 case first of
                                   IEstablish v -> if v == version
                                                    then -- We're good, (re)start the GUI.
                                                         maybeRestart first
                                                    else serverError out "Wrong client version"
                                   _            -> serverError out "Handshake failed"
  where maybeRestart f = do connId <- return 1 -- TODO: make unique connection ID using persistent client storage.
                            b <- getState cons connId
                            case b of
                              Just (i, o) -> do bUnGetIO inp f
                                                bindIO i inp
                                                bindIO o out
                                                putStrLn "Restarting.."
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
                 global <- return $ Global out nextId props events children types
                 conn <- getScreen $ global
                 -- Fire the GUI thread with the application logic.
                 actionThread <- forkIO $ main conn
                 -- Process incoming tokens.
                 processor' global `finally` killThread actionThread
        processor' :: Global -> IO ()
        processor' global = do serverInp <- bGetIO inp
                               handleServer global out serverInp
                               processor' global

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
    ISignal id name time args -> do -- TODO: implement properly
                                    es <- getHandlerState (events global) id (toEvent name)
                                    mapM_ (\x -> x (toEvent name)) es
    IKeepalive -> return () -- Do nothing at all.
    IClose -> quit
    IError msg -> do serverError out ("Client error: " ++ msg) -- TODO: Shouldn't we just close the connection, without OError?
                     quit
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

-- | Closes the connection immediately.
quit :: IO ()
quit = undefined -- TODO: close more gracefully

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
    unsafeGet (Object t i g) prop = getPropertyState (props g) i (toProp (prop $ error "Property error: undefined"))

instance EventObject Object Event where
    on (Object t i g) e f = case e of
                              (Change a) -> putHandlerState (events g) i ((ChangeEvent . toProp . a $ error "For your eyes only"), f)
                              _ -> putHandlerState (events g) i (e, f)

instance ActionAddRemove Object Object where
    add (Object t p g) (Object _ c _) = do s <- getState (children g) p
                                           case s of
                                             Just cs -> setState (children g) p $ Just (c:cs)
                                             Nothing -> setState (children g) p $ Just [c]
                                           if p >= 1000 -- TODO: should we remove this distinction?
                                             then oAction g p "add" [ObjectV [("id", IntegerV c)]]
                                             else return ()
    remove a b = return () -- TODO

instance ActionEvents Object where
    enableEvents (Object t p g) e = oAction g p "enableEvents" [IntegerV $ sum $ map eventBitmask e]
    disableEvents (Object t p g) e = oAction g p "disableEvents" [IntegerV $ sum $ map eventBitmask e]

getScreen :: Global -> IO Connection
getScreen global = return $ widgetObject (Object "Screen" 2 global)

instance NewWidget Object (ObjectT a Object) where
--  new :: [Setting t] -> o -> IO t
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


