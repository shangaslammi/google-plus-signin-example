{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHC.Generics (Generic)
import Data.Aeson (eitherDecode)
import Data.Aeson.Types
import Data.Maybe (fromMaybe)
import Data.List  (stripPrefix)
import Network.OAuth.OAuth2 (OAuth2(..), fetchAccessToken, authGetJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy  as BL
import qualified Data.ByteString.Char8 as BSC

import Control.Monad ((>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)

import Control.Error
import Control.Monad.Trans.Either

import Web.Scotty
import Network.Wai.Middleware.Static
import Network.Wai.Handler.WarpTLS
import qualified Network.Wai.Handler.Warp as Warp

newtype LoginRequest = LoginRequest ByteString

exampleApp :: OAuth2 -> ScottyM ()
exampleApp (oauth@OAuth2{..}) = do

    middleware (staticPolicy $ addBase "public")

    get "/" $ file "public/index.html"

    get "/api/clientid" $ json $ object
        [ "clientId" .= BSC.unpack oauthClientId ]

    post "/api/login" $ do
        LoginRequest code <- jsonData

        eitherT errorJson json $ do
            -- Exchange the authentication code from the client to
            -- an OAuth token.
            t    <- eitherIO $ fetchAccessToken oauth code

            -- Use the OAuth token to fetch user info.
            info <- eitherIO $
                    authGetJSON t "https://www.googleapis.com/oauth2/v1/userinfo"

            -- Return userinfo to the client.
            return $ object
                [ "userinfo" .= (info :: Value)
                ]
        where
            errorJson = json . object . return . ("error" .=) . BL.unpack
            eitherIO  = EitherT . liftIO

runServer :: OAuth2 -> IO ()
runServer = scottyApp . exampleApp >=> runTLS tlsCfg warpCfg where

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
            , "Create one according to the 'key.sample.json' file."
            ]

    parseCfg cfgFile = case eitherDecode cfgFile of
        Left e -> left $ unlines
            [ "Error parsing 'key.json':"
            , e
            ]

        Right oauth -> return oauth


-- Derive GHC.Generic instance for OAuth2 for automatic FromJSON instance
-- generation.
deriving instance Generic OAuth2

instance FromJSON OAuth2 where
    parseJSON = genericParseJSON options where
        options = defaultOptions
            { fieldLabelModifier = \s -> fromMaybe s $ stripPrefix "oauth" s }

-- We also need a FromJSON instance for ByteString in order to read the
-- key.json config file.
instance FromJSON ByteString where
    parseJSON = fmap BSC.pack . parseJSON

-- JSON parser for the client's login request
instance FromJSON LoginRequest where
    parseJSON (Object o) = fmap LoginRequest (o .: "code")
    parseJSON _          = fail "expected object"


