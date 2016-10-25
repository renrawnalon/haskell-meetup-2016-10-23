{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text, pack)
import Network.HTTP.Simple (Request, getResponseBody, httpLBS, parseRequest_)
import Web.Scotty (Parsable, ScottyM, get, raw, param, scotty, text)

someFunc :: IO ()
someFunc = scotty 8080 myScottyM

myScottyM :: ScottyM ()
myScottyM = do
    get "/" $ text "helloworld"
    get "/add/:x/:y" $ do
        (x :: Integer) <- param "x"
        y <- param "y"
        text $ pack $ show $ x + y
    get "/proxy/:url" $ do
        url <- param "url"
        liftIO $ print $ "We got url: " ++ url
        webPage <- liftIO $ fetchUrl url
        raw webPage

fetchUrl :: String -> IO ByteString
fetchUrl url = do
    let (request :: Request) = parseRequest_ ("http://" ++ url)
    response <- httpLBS request
    return $ getResponseBody response
