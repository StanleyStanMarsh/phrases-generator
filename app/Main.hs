module Main (main) where

import Lib
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Random (newStdGen)

main :: IO ()
main = do
    inputText <- TIO.readFile "input.txt"
    
    let nGrams = processText inputText
    
    TIO.putStrLn (T.pack "Enter the first word for phrase generation:")
    firstWord <- T.strip <$> TIO.getLine
    
    gen <- newStdGen
    let result = generatePhrase gen firstWord nGrams
    case result of
        Left err -> TIO.putStrLn err
        Right phrase -> do
            TIO.putStrLn (T.pack "Generated phrase:")
            TIO.putStrLn $ T.unwords phrase
    
    -- Prompt for second word
    TIO.putStrLn (T.pack "\nEnter another word for phrase generation:")
    secondWord <- T.strip <$> TIO.getLine
    
    gen2 <- newStdGen
    let result2 = generatePhrase gen2 secondWord nGrams
    case result2 of
        Left err -> TIO.putStrLn err
        Right phrase -> do
            TIO.putStrLn (T.pack "\nAnother generated phrase:")
            TIO.putStrLn $ T.unwords phrase