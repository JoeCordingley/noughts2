import Data.Functor (void)
import Test.Tasty
import Test.Tasty.HUnit
import qualified ChessSpec as ChessSpec

main :: IO ()
main = do
  chessPgns <- ChessSpec.readPgnsWithCount
  defaultMain $ testGroup "all" [ ChessSpec.tests chessPgns]

