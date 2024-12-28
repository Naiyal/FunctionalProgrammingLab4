import Test.HUnit
import LiftTest

main :: IO ()
main = do
  _ <-
    runTestTT tests
  return ()