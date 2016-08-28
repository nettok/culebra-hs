module SerDe
  ( serializeToJson
  ) where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Game

-- JSON

instance ToJSON Color
instance ToJSON Dir

instance ToJSON Pos where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = uncapitalize . Prelude.drop 3 }

instance ToJSON Snake where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = uncapitalize . Prelude.drop 5 }

instance ToJSON GameState where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = uncapitalize . Prelude.drop 2 }

uncapitalize :: String -> String
uncapitalize []     = []
uncapitalize (c:cs) = toLower c : cs

serializeToJson :: GameState -> BL.ByteString
serializeToJson = encode
