{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Control.Monad.Trans.Class (lift)

import Control.Error
import Control.Monad.Trans.Either

import Web.Scotty
import Network.Wai.Middleware.Static
import Network.Wai.Handler.WarpTLS
import qualified Network.Wai.Handler.Warp as Warp

newtype OAuthConfig = OAuthConfig { getOAuth2 :: OAuth2 } deriving Show

deriving instance Generic OAuth2

instance FromJSON ByteString where
    parseJSON = fmap BSC.pack . parseJSON

instance FromJSON OAuthConfig where
    parseJSON = fmap OAuthConfig . genericParseJSON options where
        options = defaultOptions
            { fieldLabelModifier = \s -> fromMaybe s $ stripPrefix "oauth" s }

exampleApp :: ScottyM ()
exampleApp = do
    middleware (staticPolicy $ addBase "public")

    get "/" $ file "public/index.html"

runServer :: OAuth2 -> IO ()
runServer oauth = scottyApp exampleApp >>= runTLS tlsCfg warpCfg where
    tlsCfg = defaultTlsSettings
        { certFile = "cert/server.crt"
        , keyFile  = "cert/server.key"
        }

    warpCfg = Warp.setPort 3000 $ Warp.defaultSettings


main :: IO ()
main = eitherT errLn runServer (readCfg >>= parseCfg) where

    readCfg = fmapLT fileMissing $ tryIO $ BL.readFile "key.json"

    fileMissing _ = unlines
            [ "Can't open the 'key.json' file."
            , "Create one according to 'key.sample.json' file."
            ]

    parseCfg cfgFile = case eitherDecode cfgFile of
        Left e -> left $ unlines
            [ "Error parsing 'key.json':"
            , e
            ]

        Right (OAuthConfig oauth) -> return oauth
