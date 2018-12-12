module Pot where

import Data.Time.Clock

data Variety = Tea { name :: String, steepingTime :: Int }
             deriving (Eq, Ord, Show)

data State = Empty
           | Steeping Variety UTCTime
           | Available Variety UTCTime Int

instance Show State where
  showsPrec _ Empty = showString "The teapot is empty"
  showsPrec _ (Steeping tea start) = showString "Steeping "
                                   . shows tea
                                   . showString " since "
                                   . shows start
  showsPrec _ (Available tea since amountLeft) =
                  showString "The teapot is full (" . shows amountLeft
                  . showString "l " . shows tea
                  . showString ", steeped " . shows since
                  . showString ")"


showState :: State -> UTCTime -> String
showState Empty _ = "The teapot is empty."
showState (Steeping tea start) t =
  "Steeping ...<br>Tea: " ++ name tea ++ "<br>Time left: " ++ show sec ++ " seconds"
  where
     sec :: Integer
     sec = truncate (fromIntegral (steepingTime tea) - diffUTCTime t start)
showState (Available tea start amountLeft) t =
  "Tea: " ++ name tea ++ "<br>" ++
  "Cups left: " ++ show amountLeft ++ -- ++ replicate amountLeft '☕' ++ "<br>" ++
  "<br>Brewed " ++ show min ++ units
  where
    min :: Integer
    min = truncate (diffUTCTime t start / 60)
    units | min == 1 = " minute ago"
          | otherwise = " minutes ago"

cPOT_CAPACITY :: Int
cPOT_CAPACITY = 6

startBrewing :: Variety -> UTCTime -> State
startBrewing variety start = Steeping variety start

checkState :: State -> UTCTime -> Maybe State
checkState (Steeping tea start) now | diffUTCTime now start >= fromIntegral (steepingTime tea)
                                         = Just (Available tea now cPOT_CAPACITY)
checkState _ _ = Nothing

setCups :: Int -> State -> State
setCups n (Available v t _) | n <= 0 = Empty
                            | otherwise = Available v t n
setCups _ st = st
