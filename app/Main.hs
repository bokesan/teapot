{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.Time.Clock
import Data.IORef
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text

import qualified Pot

main :: IO ()
main = do
    state <- newIORef Pot.clean
    run 8090 (app' state)

app' :: IORef Pot.State -> Application
app' ref req answer =
  case requestMethod req of
    "GET"  -> handleGet  ref []             answer
    "POST" -> handlePost ref (pathInfo req) answer
    "PUT"  -> handlePut  ref (pathInfo req) answer
    _      -> answer notFound

handleGet, handlePut, handlePost :: IORef Pot.State -> [Text] -> (Response -> IO ResponseReceived) -> IO ResponseReceived

handleGet ref _ answer = do
  now <- getCurrentTime
  resp <- atomicModifyIORef' ref (getState now)
  answer (page (BS.pack resp))

handlePut ref [n] answer = do
  success <- atomicModifyIORef' ref (serveCups (read (unpack n)))
  answer (if success then ok else notFound)
handlePut _ _ answer = answer notFound

serveCups :: Int -> Pot.State -> (Pot.State, Bool)
serveCups cups = maybeToPair $ Pot.serve cups

brew :: Int -> String -> Int -> UTCTime -> Pot.State -> (Pot.State, Bool)
brew cups tea minutes now =
  maybeToPair $ Pot.startBrewing (Pot.Tea tea (minutes * 60 - 20)) cups now

maybeToPair :: (a -> Maybe a) -> a -> (a, Bool)
maybeToPair f x = case f x of
                    Nothing -> (x, False)
                    Just x' -> (x', True)

handlePost ref [n1, tea, n2] answer = do
  let cups = read (unpack n1)
  let minutes = read (unpack n2)
  now <- getCurrentTime
  success <- atomicModifyIORef ref (brew cups (unpack tea) minutes now)
  answer (if success then plain "Started steeping..." else notFound)
handlePost _ _ answer = answer notFound


getState :: UTCTime -> Pot.State -> (Pot.State, String)
getState now st = let st' = Pot.getState now st in
                  (st', Pot.showState (Pot.pot st') now)

plain :: BS.ByteString -> Response
plain text = responseLBS status200
    [("Content-Type", "text/plain")]
    (BS.concat [text, "\n"])

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found\n"

ok :: Response
ok = plain "OK"

page :: BS.ByteString -> Response
page text = responseLBS
              status200
              [("Content-type", "text/html")]
              (BS.concat ["<!DOCTYPE html>\n<html><head><title>Tea</title>",
                          "<meta http-equiv='refresh' content='10'>",
                          "<meta charset='UTF-8'>",
                          "</title></head><body><h1>Chris' Teapot</h1><p>",
                          text,
                          "</p>\n<hr><p align='center'><i>Served by TeaPot 0.1</i></p>",
                          "</body></html>\n"])
            
