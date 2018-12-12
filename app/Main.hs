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
    state <- newIORef Pot.Empty
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
  _ <- atomicModifyIORef' ref (\st -> (Pot.setCups (read (unpack n)) st, ()))
  answer ok
handlePut _ _ answer = answer notFound

handlePost ref [n, tea] answer = do
  now <- getCurrentTime
  atomicWriteIORef ref (Pot.startBrewing (Pot.Tea (unpack tea) (60 * read (unpack n) - 20)) now)
  answer (plain "Sterted steeping ...")
handlePost _ _ answer = answer notFound


getState :: UTCTime -> Pot.State -> (Pot.State, String)
getState now st = case Pot.checkState st now of
                    Nothing  -> (st, Pot.showState st now)
                    Just st' -> (st', Pot.showState st' now)

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
            
