{-# LANGUAGE OverloadedStrings #-}

module TestSpeed where

import Chess.Notation.Parser as Parser
import Control.Applicative (many)
import Data.Attoparsec.Text (IResult (..), endOfInput, parse)
import qualified Data.Text.IO as T
import System.CPUTime (getCPUTime)
import Text.Printf

main :: IO ()
main = do
  fileText <- T.readFile "pgns/moves"
  start <- getCPUTime
  case parse (many (Parser.lexeme Parser.parseMoveWithAppendation) <* endOfInput) fileText of
    Done "" games -> do
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10 ^ 12) :: Double
      printf "Parsed %d games in %0.3f sec\n" (length games) diff
    err -> print err
