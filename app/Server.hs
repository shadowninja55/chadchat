{-# LANGUAGE ViewPatterns #-}
module Main where

import ChadChat.Message
import Codec.Serialise (serialise)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Logging qualified as L
import Control.Monad
import Data.ByteString.UTF8 (toString)
import Data.Connection
import Data.Function (fix)
import Data.Text qualified as T
import Network.Socket (SockAddr)
import System.IO.Streams.TCP
import System.IO.Streams qualified as S

handleClient :: Chan (Message, SockAddr) -> TCPConnection -> IO ()
handleClient messages conn = do
  Just (toString -> username) <- S.read $ source conn
  let (_, addr) = connExtraInfo conn
  L.log . T.pack $ show addr <> " connected"
  void . forkIO $ forever do
    (message, sender) <- readChan messages
    when (addr /= sender) $
      send conn $ serialise message
  fix \loop -> S.read (source conn) >>= \case
    Just (toString -> content) -> do
      L.debug . T.pack . display $ Message (show addr) content
      writeChan messages (Message username content, addr)
      loop
    Nothing -> close conn

main :: IO ()
main = L.withStdoutLogging do
  L.log . T.pack $ "running on port " <> show port
  messages <- newChan
  sock <- bindAndListen 64 port
  forever do
    conn <- accept sock
    messages' <- dupChan messages
    handleClient messages' conn
 where
  port = 5555
