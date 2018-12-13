{-# LANGUAGE OverloadedStrings #-}
module Pot where

import Data.Time.Clock
import Text.Blaze.Html5 as H


data Variety = Tea { name :: String, steepingTime :: !Int }
             deriving (Eq, Ord, Show)

data State = State { pot :: !PotState
                   , cupsServed :: !Int
                   }
             deriving (Eq, Show)

data PotState = Empty
              | Steeping { tea :: Variety, since :: !UTCTime, cups :: !Int }
              | Available { tea :: Variety, since :: !UTCTime, cups :: !Int }
              deriving (Eq, Show)

clean :: State
clean = State{ pot = Empty, cupsServed = 0 }

toHtml :: UTCTime -> PotState -> Html

toHtml _ Empty = p "The teapot is empty."

toHtml now (Steeping tea_ since_ _) =
  let sec :: Integer
      sec = truncate (fromIntegral (steepingTime tea_) - diffUTCTime now since_)
  in
  p $ do "Steeping..."
         br
         "Tea: "
         string (name tea_)
         br
         "Time left: "
         H.toHtml sec
         " seconds"

toHtml now (Available tea_ since_ cups_) =
  let
    minutes :: Integer
    minutes = truncate (diffUTCTime now since_ / 60)
    units | minutes == 1 = " minute ago"
          | otherwise    = " minutes ago"
  in
  p $ do "Tea: "
         string (name tea_)
         br
         "Cups left: "
         H.toHtml cups_
         br
         "Brewed "
         H.toHtml minutes
         units

cPOT_CAPACITY :: Int
cPOT_CAPACITY = 6

startBrewing :: Variety -> Int -> UTCTime -> State -> Maybe State
startBrewing tea_ cups_ start state =
  case pot state of
    Empty -> Just state{pot = Steeping tea_ start cups_}
    _     -> Nothing

getState :: UTCTime -> State -> State
getState now state =
  case pot state of
    Steeping tea_ since_ cups_ | diffUTCTime now since_ >= fromIntegral (steepingTime tea_)
                                 -> state{pot = Available tea_ now cups_}
    _ -> state

serve :: Int -> State -> Maybe State
serve cups_ state =
  case pot state of
    Available tea_ since_ cupsLeft
      | cups_ < cupsLeft  -> Just state{ pot = Available tea_ since_ (cupsLeft - cups_)
                                       , cupsServed = cupsServed state + cups_ }
      | cups_ == cupsLeft -> Just state{ pot = Empty
                                       , cupsServed = cupsServed state + cups_ }
    _  -> Nothing
