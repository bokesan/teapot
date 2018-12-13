import Test.Tasty
import Test.Tasty.HUnit
import Data.Time.Clock

import Pot

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]


sampleState :: IO Pot.State
sampleState = do
  now <- getCurrentTime
  return Pot.State { pot = Pot.Available Pot.Tea{name="Oolong", steepingTime=100} now 3, cupsServed = 5 }

unitTests = testGroup "Unit tests"
  [ testCase "serving more that available fails" $ do
      st <- sampleState
      let st' = Pot.serve 100 st
      assertEqual "no result" Nothing st'

  , testCase "get changes state" $ do
      now <- getCurrentTime
      let Just st = Pot.startBrewing Pot.Tea{name="T", steepingTime=40} 6 now Pot.clean
      assertEqual "tea brewing"
        (Pot.State { pot = Pot.Steeping Pot.Tea{name="T", steepingTime=40} now 6, cupsServed = 0 })
        (Pot.getState (addUTCTime 20 now) st)
      let now' = addUTCTime 60 now
      assertEqual "tea ready"
        (Pot.State { pot = Pot.Available Pot.Tea{name="T", steepingTime=40} now' 6, cupsServed = 0 })
        (Pot.getState now' st)

  , testCase "serving " $ do
      st <- sampleState
      let Just st' = Pot.serve 2 st
      assertEqual "available decremented" 1 (Pot.cups (Pot.pot st'))
      assertEqual "cups served incremented" 7 (Pot.cupsServed st')
  ]
