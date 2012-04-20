{-# OPTIONS_GHC -XMultiParamTypeClasses -XFlexibleContexts #-}

-- | The actual GUI implementation, without the server part.

module GUI (
        module Widgets,
        module Properties,
        
        processor,
        InputToken,
        OutputToken,
        
        Connection,
        newButton,
        newWindow,
        newEntry,
        
        get,
        set,
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

import Widgets hiding (Screen, obj, newObject)
import qualified Widgets as W
import Properties hiding (Prop (..), sameProp)
import qualified Properties as P
import Types
import Buffer
import Tokens

-- | GUI version
version :: Version
version = "1.0"

type Parent = Maybe Identifier
type Connection = W.Screen () Object
type Type = String
type EventHandler = Event -> IO ()

data Global = Global { out :: Buffer ActionToken, nextId :: MVar Identifier, props :: State [P.Prop], events :: State [(Event, EventHandler)] }
data Object = Object Type Identifier Global

data ActionToken
  = AUpdate Identifier P.Prop
  | ANew Identifier Type


type State a = HashTable Identifier a
newState :: IO (State a)
newState = HT.fromList (HT.hashInt . fromInteger) []

getState :: State a -> Identifier -> IO (Maybe a)
getState state i = HT.lookup state i

setState :: State a -> Identifier -> Maybe a -> IO ()
setState state i (Just s) = HT.insert state i s
setState state i Nothing  = HT.delete state i

processor :: (Connection -> IO ()) -> Buffer InputToken -> Buffer OutputToken -> IO ()
processor main inp out = do first <- bGetIO inp
                            case first of
                              IEstablish v -> if v == version
                                               then (bPutIO out $ OAcknowledge version) >> run
                                               else serverError out "Wrong client version"
                              _            -> serverError out "Handshake failed"
  where run = do props <- newState
                 events <- newState
                 actions <- newBuffer
                 nextId <- newMVar 1000
                 conn <- getScreen $ Global actions nextId props events
                 actionThread <- forkIO $ main conn
                 processor' props events actions `finally` killThread actionThread
        processor' :: State [P.Prop] -> State [(Event, EventHandler)] -> Buffer ActionToken -> IO ()
        processor' props events actions = do (actionInp, serverInp) <- bGet2IO (actions, inp)
                                             mapM_ (handleServer props events out) $ mapMaybe id [serverInp]
                                             mapM_ (handleAction props out) $ mapMaybe id [actionInp]
                                             processor' props events actions

handleServer :: State [P.Prop] -> State [(Event, EventHandler)] -> Buffer OutputToken -> InputToken -> IO ()
handleServer props events out token =
  case token of
    IEstablish v -> serverError out "Connection already established."
    ISignal id name time args -> do -- TODO: implement properly
                                    es <- getHandlerState events id (toEvent name)
                                    mapM_ (\x -> x (toEvent name)) es
    IKeepalive -> return () -- Do nothing at all.
    IClose -> quit
    IError msg -> do serverError out ("Client error: " ++ msg) -- Shouldn't we just close the connection, without OError?
                     quit
    ISet id name value -> case fromTuple (name, value) of
                            Just prop -> do putPropertyState True props id prop
                                            es <- getHandlerState events id (ChangeEvent prop)
                                            mapM_ (\x -> x undefined) es
                            Nothing -> serverError out "Client tried to set incorrect property"
    IUnknown    -> serverError out "Received unrecognized token"

serverError :: Buffer OutputToken -> String -> IO ()
serverError out s = do putStrLn ("ERROR: " ++ s)
                       bPutIO out $ OError s

quit :: IO ()
quit = undefined -- TODO: close more gracefully

toEvent :: String -> Event
toEvent "Motion" = MotionEvent
toEvent "Scroll" = ScrollEvent
toEvent "Enter" = EnterEvent
toEvent "Leave" = LeaveEvent
toEvent "KeyPress" = KeyPressEvent
toEvent "KeyRelease" = KeyReleaseEvent
toEvent "ButtonPress" = ButtonPressEvent
toEvent "ButtonRelease" = ButtonReleaseEvent
toEvent "Focus" = FocusEvent
toEvent "Blur" = BlurEvent

fromTuple :: (String, Value) -> Maybe P.Prop
fromTuple t = case t of
                     ("Label", StringV v) -> Just $ P.LabelProp $ Label v
                     ("Text", StringV v)  -> Just $ P.TextProp $ Text v
                     -- TODO: append
                     _ -> Nothing

toTuple :: P.Prop -> (String, Value)
toTuple p = case p of
              P.VisibleProp (Visible v) -> ("Visible", BoolV v)
              P.SizeProp (Size (a, b)) -> ("Size", ListV $ map IntegerV [a, b])
              P.MarginProp (Margin (a, b, c, d)) -> ("Margin", ListV $ map IntegerV [a, b, c, d])
              P.SensitiveProp (Sensitive v) -> ("Sensitive", BoolV v)
              P.FocusProp (Focus v) -> ("Focus", BoolV v)
              P.TitleProp (Title v) -> ("Title", StringV v)
              P.OpacityProp (Opacity v) -> ("Opacity", FloatV v)
              P.LabelProp (Label v) -> ("Label", StringV v)
              P.ParentProp (Parent v) -> ("Parent", IntegerV v)
              P.TextProp (Text v) -> ("Text", StringV v)
              P.EditableProp (Editable v) -> ("Editable", BoolV v)
              P.VisibilityProp (Visibility v) -> ("Visibility", BoolV v)
              P.MaxLengthProp (MaxLength v) -> ("MaxLength", IntegerV v)
              P.EventsProp (Events v) -> ("Events", ListV $ map (StringV . show) v)
              

-- TODO: remove the actiontoken part entirely as it is redundant
handleAction :: State [P.Prop] -> Buffer OutputToken -> ActionToken -> IO ()
handleAction state out token =
  case token of
    AUpdate i f              -> let (p, v) = toTuple f
                                in bPutIO out $ OSet i p v
--    AUpdate i (ParentProp p) -> do
    ANew i t                 -> bPutIO out $ OCreate i t

putToken :: Global -> ActionToken -> IO ()
putToken global token = bPutIO (out global) $ token

putPropertyState :: Bool -> State [P.Prop] -> Identifier -> P.Prop -> IO ()
putPropertyState safe s i p = do mps <- getState s i
                                 case mps of
                                   Just ps -> case span (not . P.sameProp p) ps of
                                               (a, []) -> if safe
                                                           then return ()
                                                           else setState s i $ Just (p:a)
                                               (a, (_:b)) -> setState s i $ Just (p:a ++ b)
                                   Nothing -> if safe
                                               then return ()
                                               else setState s i $ Just [p]

getPropertyState :: State [P.Prop] -> Identifier -> P.Prop -> IO P.Prop
getPropertyState s i p = do mps <- getState s i
                            case mps of
                                   Just ps -> case span (not . P.sameProp p) ps of
                                               (a, []) -> error "Property error: no such property"
                                               (a, (x:b)) -> return x
                                   Nothing -> error "Property error: no such object"

putHandlerState :: State [(Event, EventHandler)] -> Identifier -> (Event, EventHandler) -> IO ()
putHandlerState s i e = do mes <- getState s i
                           case mes of
                             Just es -> setState s i $ Just (e:es)
                             Nothing -> setState s i $ Just [e]

getHandlerState :: State [(Event, EventHandler)] -> Identifier -> Event -> IO [EventHandler]
getHandlerState s i e = do mes <- getState s i
                           case mes of
                                  Just es -> return $ map snd . filter (\x -> fst x == e) $ es
                                  Nothing -> return []

instance GUIObject Object P.Prop where
    setProperty (Object t i g) prop = do putPropertyState False (props g) i (toProp prop)
                                         putToken g $ AUpdate i (toProp prop)
    getProperty (Object t i g) prop = getPropertyState (props g) i (toProp (prop $ error "Property error: undefined"))
    addChildObject (Object t p g) c = putToken g $ AUpdate (getIdentifier c) (P.ParentProp $ Parent p)

instance IdObject Object where
    getIdentifier (Object _ i _) = i

instance EventObject Object Event where
    on (Object t i g) e f = case e of
                              (Change a) -> putHandlerState (events g) i ((ChangeEvent . toProp . a $ error "For your eyes only"), f)
                              _ -> putHandlerState (events g) i (e, f)

getScreen :: Global -> IO Connection
getScreen global = do -- putToken global $ ANew 2 "Screen" -- TODO: should we send this?
                      return $ W.newObject (Object "Screen" 2 global)

newChild t ds p = let Object _ _ g = W.obj p
                  in do i <- getNextId g
                        let o = W.newObject $ Object t i g
                         in do putToken g $ ANew i t
                               addChildObject p o
                               mapM_ (\x -> case x of
                                              P.VisibleProp v -> setProperty (W.obj o) v
                                              P.SizeProp v -> setProperty (W.obj o) v
                                              P.MarginProp v -> setProperty (W.obj o) v
                                              P.SensitiveProp v -> setProperty (W.obj o) v
                                              P.FocusProp v -> setProperty (W.obj o) v
                                              P.TitleProp v -> setProperty (W.obj o) v
                                              P.OpacityProp v -> setProperty (W.obj o) v
                                              P.LabelProp v -> setProperty (W.obj o) v
                                              P.TextProp v -> setProperty (W.obj o) v
                                              P.EditableProp v -> setProperty (W.obj o) v
                                              P.VisibilityProp v -> setProperty (W.obj o) v
                                              P.MaxLengthProp v -> setProperty (W.obj o) v
                                              P.EventsProp v -> setProperty (W.obj o) v
                                 ) ds
                               return o

getNextId :: Global -> IO Identifier
getNextId global = let m = nextId global
                   in do i <- takeMVar m
                         putMVar m (i + 1)
                         return i

newWindow :: Connection -> IO (Window () Object)
newWindow = newChild "Window" windowDefaults

newButton :: Container a Object -> IO (Button () Object)
newButton = newChild "Button" buttonDefaults

newEntry :: Container a Object -> IO (Entry () Object)
newEntry = newChild "Entry" entryDefaults

