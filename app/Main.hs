module Main (main) where

import qualified Data.Text.IO as TIO

import Lib

main :: IO ()
main = do
    result <- TIO.readFile "test.txt"
    print (runParser allSentences result)