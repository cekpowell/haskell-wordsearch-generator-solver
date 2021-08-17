module Main where

-----------------------
-- IMPORT STATEMENTS -- 
-----------------------

import System.IO
import System.Environment
import Data.List.Split
import Solver

-----------------
-- MAIN METHOD --
-----------------
 
-- main
        -- @brief:
        -- @params: 
        -- @return:
main :: IO ()
main = do
            -- Gathering grid --
            (wordsList : gridList : _) <- getArgs -- name of program being run
            let words = splitOn "," wordsList
            let grid  = splitOn "," gridList

            -- solving grid
            let solution = solveWordSearch words grid

            -- printing generated grid
            putStrLn (show solution)