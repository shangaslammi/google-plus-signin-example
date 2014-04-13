{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

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

import Control.Error.Util
import Control.Monad.Trans.Either

newtype OAuthConfig = OAuthConfig { getOAuth2 :: OAuth2 } deriving Show

deriving instance Generic OAuth2

instance FromJSON ByteString where
    parseJSON = fmap BSC.pack . parseJSON

instance FromJSON OAuthConfig where
    parseJSON = fmap OAuthConfig . genericParseJSON options where
        options = defaultOptions
            { fieldLabelModifier = \s -> fromMaybe s $ stripPrefix "oauth" s }

main :: IO ()
main = eitherT fileMissing parseCfg readCfg where

    readCfg = tryIO $ BL.readFile "key.json"

    fileMissing _ = do
        errLn "Can't open the 'key.json' file."
        errLn "Create one according to 'key.sample.json' file."

    parseCfg cfgFile = case eitherDecode cfgFile of
        Left e -> do
            errLn "Error parsing 'key.json':"
            errLn e
        Right (OAuthConfig oauth) -> print oauth
