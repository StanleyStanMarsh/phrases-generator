module Main (main) where

import Lib
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Random (newStdGen)

main :: IO ()
main = do
    test <- TIO.readFile "test.txt"
    input1 <- TIO.readFile "input.txt"
    input2 <- TIO.readFile "input2.txt"

    case runParser allSentencesAsText test of
      Nothing -> putStrLn "Error"
      Just (remaining, processed) -> writeTextListToFile "sentences_test.txt" processed 

    case runParser allSentencesAsText input1 of
      Nothing -> putStrLn "Error"
      Just (remaining, processed) -> writeTextListToFile "sentences1.txt" processed 

    case runParser allSentencesAsText input2 of
      Nothing -> putStrLn "Error"
      Just (remaining, processed) -> writeTextListToFile "sentences2.txt" processed
    
    let nGramsTest = processText test
    let nGrams1 = processText input1
    let nGrams2 = processText input2

    writeDictionaryToFile "dictionary_test.txt" nGramsTest
    writeDictionaryToFile "dictionary1.txt" nGrams1 
    writeDictionaryToFile "dictionary2.txt" nGrams2

    -- print nGrams1
    -- print "\n"
    -- print nGrams2
    -- print "\n"
    
    TIO.putStrLn (T.pack "Enter the first word for dialogue:")
    firstWord <- T.strip <$> TIO.getLine
    
    TIO.putStrLn (T.pack "Enter the number of exchanges:")
    depthStr <- getLine
    let depth = (read depthStr :: Int) - 1
    
    gen <- newStdGen
    let dialogue = generateDialogue gen firstWord nGrams1 nGrams2 depth
    TIO.putStrLn (T.pack "\nGenerated dialogue:")
    mapM_ (printDialogueTurn . formatDialogueTurn) dialogue
  where
    formatDialogueTurn (speaker, response) = 
        (T.pack $ "Model " ++ show speaker ++ ": ", 
         case response of
            Left err -> err
            Right phrase -> T.unwords phrase)
    
    printDialogueTurn (prefix, message) = 
        TIO.putStr prefix >> TIO.putStrLn message