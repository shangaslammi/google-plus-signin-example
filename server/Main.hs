{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics (Generic)
import Data.Aeson (eitherDecode)
import Data.Aeson.Types
import Data.Maybe (fromMaybe)
import Data.List  (stripPrefix)
import Network.OAuth.OAuth2 (OAuth2(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy  as BL
import qualified Data.ByteString.Char8 as BSC

newtype OAuthConfig = OAuthConfig { getOAuth2 :: OAuth2 } deriving Show

deriving instance Generic OAuth2

instance FromJSON ByteString where
    parseJSON = fmap BSC.pack . parseJSON

instance FromJSON OAuthConfig where
    parseJSON = fmap OAuthConfig . genericParseJSON options where
        options = defaultOptions
            { fieldLabelModifier = \s -> fromMaybe s $ stripPrefix "oauth" s }

main :: IO ()
main = do
    cfgFile <- BL.readFile "key.sample.json"
    let cfg = eitherDecode cfgFile :: Either String OAuthConfig
    print cfg
