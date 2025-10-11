import Data.Functor (void)
import Test.HUnit

main :: IO ()
main = void $ runTestTT tests

tests = test [1.0 ~=? 1.0]
