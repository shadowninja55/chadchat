{-# LANGUAGE DeriveAnyClass #-}
module ChadChat.Message where

import Codec.Serialise (Serialise)
import GHC.Generics (Generic)

data Message = Message
  { author :: String
  , content :: String 
  }
  deriving (Generic, Serialise, Show)

display :: Message -> String
display (Message author content) = "[" <> author <> "] " <> content
