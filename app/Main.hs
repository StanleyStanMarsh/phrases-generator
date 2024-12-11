module Main (main) where

import Lib
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Random (newStdGen)

main :: IO ()
main = do

    input1 <- TIO.readFile "input.txt"
    input2 <- TIO.readFile "input2.txt"
    
    let nGrams1 = processText input1
    let nGrams2 = processText input2

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