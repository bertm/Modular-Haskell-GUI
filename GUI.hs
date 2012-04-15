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
        set
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
version = 0.1

type Parent = Maybe Identifier
type Connection = W.Screen () Object
type Type = String

data Global = Global { out :: Buffer ActionToken, nextId :: MVar Identifier, props :: State [P.Prop] }
data Object = Object Type Identifier Global

data ActionToken
  = AUpdate Identifier P.Prop
  | ANew Identifier Type


type State a = HashTable Identifier a
newState :: IO (State a)
newState = HT.fromList HT.hashInt []

getState :: State a -> Int -> IO (Maybe a)
getState state i = HT.lookup state i

setState :: State a -> Int -> Maybe a -> IO ()
setState state i (Just s) = HT.insert state i s
setState state i Nothing  = HT.delete state i

processor :: (Connection -> IO ()) -> Buffer InputToken -> Buffer OutputToken -> IO ()
processor main inp out = do props <- newState
                            actions <- newBuffer
                            nextId <- newMVar 1
                            conn <- getScreen $ Global actions nextId props
                            actionThread <- forkIO $ main conn
                            processor' props actions `finally` killThread actionThread
  where processor' :: State [P.Prop] -> Buffer ActionToken -> IO ()
        processor' props actions = do (actionInp, serverInp) <- bGet2IO (actions, inp)
                                      mapM_ (handleServer props out) $ mapMaybe id [serverInp]
                                      mapM_ (handleAction props out) $ mapMaybe id [actionInp]
                                      processor' props actions

handleServer :: State [P.Prop] -> Buffer OutputToken -> InputToken -> IO ()
handleServer props out token =
  case token of
    IEstablish v -> if v == version
                     then bPutIO out $ OAcknowledge version
                     else serverError out ("Wrong client version: " ++ show v)
    ISignal id name time args -> return () -- TODO: implement
    IKeepalive -> return () -- Do nothing at all.
    IClose -> quit
    IError msg -> do serverError out ("Client error: " ++ msg)
                     quit
    ISet id name value -> case fromTuple (name, value) of
                            Just prop -> putPropertyState True props id prop
                            Nothing -> serverError out "Client tried to set incorrect property"
    IUnknown    -> serverError out "Received unrecognized token"

serverError :: Buffer OutputToken -> String -> IO ()
serverError out s = do putStrLn ("ERROR: " ++ s)
                       bPutIO out $ OError s

quit :: IO ()
quit = undefined -- TODO: close more gracefully

fromTuple :: (String, String) -> Maybe P.Prop
fromTuple (n, v) = case n of
                     "Label" -> Just $ P.LabelProp $ Label v
                     "Text" -> Just $ P.TextProp $ Text v
                     -- TODO: append
                     _ -> Nothing

toTuple :: P.Prop -> (String, String)
toTuple p = case p of
              P.VisibleProp (Visible v) -> ("Visible", show v)
              P.SizeProp (Size (a, b)) -> ("Size", show a ++ " " ++ show b)
              P.MarginProp (Margin (a, b, c, d)) -> ("Margin", show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d)
              P.SensitiveProp (Sensitive v) -> ("Sensitive", show v)
              P.FocusProp (Focus v) -> ("Focus", show v)
              P.TitleProp (Title v) -> ("Title", v)
              P.OpacityProp (Opacity v) -> ("Opacity", show v)
              P.LabelProp (Label v) -> ("Label", v)
              P.ParentProp (Parent v) -> ("Parent", show v)
              P.TextProp (Text v) -> ("Text", v)
              P.EditableProp (Editable v) -> ("Editable", show v)
              P.VisibilityProp (Visibility v) -> ("Visibility", show v)
              P.MaxLengthProp (MaxLength v) -> ("MaxLength", show v)
              P.EventsProp (Events v) -> ("Events", show v)
              

-- TODO: remove the actiontoken part entirely as it is redundant
handleAction :: State [P.Prop] -> Buffer OutputToken -> ActionToken -> IO ()
handleAction state out token =
  case token of
    AUpdate i f   -> let (p, v) = toTuple f
                     in bPutIO out $ OSet i p v
    ANew i t    -> bPutIO out $ OCreate i t

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

instance GUIObject Object P.Prop where
    setProperty (Object t i g) prop = do putPropertyState False (props g) i (toProp prop)
                                         putToken g $ AUpdate i (toProp prop)
    getProperty (Object t i g) prop = getPropertyState (props g) i (toProp (prop $ error "Property error: undefined"))
    addChildObject (Object t p g) c = putToken g $ AUpdate (getIdentifier c) (P.ParentProp $ Parent p)

instance IdObject Object where
    getIdentifier (Object _ i _) = i

getScreen :: Global -> IO Connection
getScreen global = do i <- getNextId global
                      putToken global $ ANew i "Screen"
                      return $ W.newObject (Object "Screen" i global)

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

