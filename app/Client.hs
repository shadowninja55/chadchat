{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Edit
import ChadChat.Message
import Codec.Serialise
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as B
import Data.ByteString.Char8 (pack)
import Data.Connection
import Data.Function (fix)
import Data.Generics.Labels ()
import Data.Text.Zipper (clearZipper)
import GHC.Generics (Generic)
import Graphics.Vty qualified as V
import System.Environment (getArgs)
import System.IO.Streams.TCP
import System.IO.Streams qualified as S

data State = State 
  { username :: String
  , messages :: [Message]
  , messageBox :: Editor String ()
  , connection :: TCPConnection
  }
  deriving Generic

draw :: State -> [Widget ()]
draw st = pure $ vBox 
  [ withBorderStyle unicodeRounded . border . vBox . reverse $ 
      fill ' ' : (str . display <$> st.messages)
  , withBorderStyle unicodeRounded . border $
      renderEditor (str . head) True st.messageBox
  ]

onEvent :: State -> BrickEvent () Message -> EventM () (Next State)
onEvent st = \case
  AppEvent m -> continue $ st & #messages %~ (m :)
  VtyEvent (V.EvKey V.KEsc []) -> halt st
  VtyEvent (V.EvKey V.KEnter []) -> do
    let 
     content = head $ getEditContents st.messageBox
     message = Message st.username content
    liftIO . send st.connection . B.fromStrict . pack $ content
    continue $ st 
      & (#messageBox %~ applyEdit clearZipper)
      . (#messages %~ (message :))
  VtyEvent ev -> continue =<< handleEventLensed st #messageBox handleEditorEvent ev
  _ -> continue st

main :: IO ()
main = do
  let builder = V.mkVty V.defaultConfig
  handle <- builder
  inbound <- newBChan 10
  [username, host, read -> port] <- getArgs
  conn <- connect host port
  send conn . B.fromStrict $ pack username
  void . forkIO $ fix \loop -> S.read (source conn) >>= \case
    Just content -> do
      writeBChan inbound . deserialise . B.fromStrict $ content
      loop
    Nothing -> pure ()
  let st = State username [] (editor () (Just 1) "") conn
  void $ customMain handle builder (Just inbound) app st
 where
  app :: App State Message ()
  app = App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = onEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap V.defAttr []
    }
