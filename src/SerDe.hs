module SerDe
  ( serializeToJson
  ) where

import Data.Aeson
import Data.ByteString.Lazy as BL
import Game

-- JSON

instance ToJSON Color
instance ToJSON Dir
instance ToJSON Pos
instance ToJSON Snake
instance ToJSON GameState

instance FromJSON Color
instance FromJSON Dir
instance FromJSON Pos
instance FromJSON Snake
instance FromJSON GameState

serializeToJson :: GameState -> BL.ByteString
serializeToJson = encode
