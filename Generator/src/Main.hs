module Main where

-----------------------
-- IMPORT STATEMENTS -- 
-----------------------

import System.IO
import System.Environment
import Data.List.Split
import Generator

-----------------
-- MAIN METHOD --
-----------------

-- main
        -- @brief:
        -- @params: 
        -- @return:
main :: IO ()
main = do
            -- Gathering run time parameters
            (wordList : densityString : _ ) <- getArgs -- name of program being run
            let words   = splitOn "," wordList
            let density = read densityString

            -- generating new wordsearch puzzle
            grid <- createWordSearch words density

            -- printing generated grid
            printGrid grid
            


------------------------
-- PRINTING FUNCTIONS -- 
------------------------

-- printGrid
        -- @brief:
        -- @params: 
        -- @return:
printGrid :: WordSearchGrid -> IO ()
printGrid [] = return ()
printGrid (w:ws) = do printRow w
                      putStrLn ""
                      printGrid ws

-- printRow
        -- @brief:
        -- @params: 
        -- @return:
printRow :: [Char] -> IO ()
printRow [] = return ()
printRow (l:ls) = do putChar l 
                     putStr " "
                     printRow ls