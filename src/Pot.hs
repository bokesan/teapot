module Pot where

import Data.Time.Clock

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

showState :: PotState -> UTCTime -> String
showState Empty _ = "The teapot is empty."
showState (Steeping tea_ start _) t =
  "Steeping ...<br>Tea: " ++ name tea_ ++ "<br>Time left: " ++ show sec ++ " seconds"
  where
     sec :: Integer
     sec = truncate (fromIntegral (steepingTime tea_) - diffUTCTime t start)
showState (Available tea_ start amountLeft) t =
  "Tea: " ++ name tea_ ++ "<br>" ++
  "Cups left: " ++ show amountLeft ++ -- ++ replicate amountLeft '☕' ++ "<br>" ++
  "<br>Brewed " ++ show minutes ++ units
  where
    minutes :: Integer
    minutes = truncate (diffUTCTime t start / 60)
    units | minutes == 1 = " minute ago"
          | otherwise    = " minutes ago"

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
