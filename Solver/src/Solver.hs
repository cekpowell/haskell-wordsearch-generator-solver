-- COMP2209 Functional Programming Challenges
-- Author: Charles Powell
-- (c) University of Southampton 2020

module Solver
    ( WordSearchGrid,Placement,Posn,Orientation(..),
    solveWordSearch
    ) where


-----------------------
-- IMPORT STATEMENTS -- 
-----------------------


import System.Random
import Data.List
import Data.Char


-----------------------
-- DATA DEFINITIONS  -- 
-----------------------


type WordSearchGrid = [[ Char ]]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read)



---------------------
-- START OF SOLVER --
---------------------


{- solveWordSearch
    @brief: Main function for challenge. Provides solutions for a word search puzzle.
            Main Logic:
                - Recurse through the given list of words, and for each word:
                    - Recurse through the grid, looking for the character at the
                      start of the word.
                    - When it is found, check if the word is found at this position
                      by checking all possible orientations.
                    - If it is, return the placement, if not, move on to the next 
                      position in the grid.
                    - If a word isnt found, return nothing.
                - Return the list of all word-placement pairings.
    @param: [String]: The words being searched for.
            WordSearchGrid: The grid being solved.
    @return: The list of placements for all words.
-}
solveWordSearch :: [ String ] -> WordSearchGrid -> [ (String,Maybe Placement) ]
solveWordSearch words grid | ifValidInputGrid = solveWordSearchAux uWords uGrid []
                           | otherwise = error "Invalid input grid: must be (n x n) grid."
        where  
                ifValidInputGrid = checkInputGrid grid (length grid)
                uWords           = map (map toUpper) words
                uGrid            = map (map toUpper) grid

{- solveWordSearchAux
    @brief: Auxilliary function that recursivley checks for each word in the grid.
    @param: [String]: Words being searched for in the grid.
            WordSearchGrid: The grid being searched in.
            [(String, Maybe Placement)]: The current list of placements
    @return: The list of maybe placements for all given words.
-}
solveWordSearchAux :: [String] -> WordSearchGrid -> [(String, Maybe Placement)] -> [(String, Maybe Placement)]
solveWordSearchAux [] grid placements             = placements -- base case, all words checked for
solveWordSearchAux (cw:wordsLeft) grid placements = solveWordSearchAux -- recursive case, need to check if another word is in the grid
                                                        wordsLeft 
                                                        grid 
                                                        (placements ++ [(cw, (checkIfWordInGrid cw grid))])

checkInputGrid :: WordSearchGrid -> Int -> Bool
checkInputGrid [] dimension = True
checkInputGrid (firstRow:restOfGrid) dimension | (length firstRow) /= dimension = False
                                               | otherwise                      = checkInputGrid 
                                                                                        restOfGrid 
                                                                                        dimension

{- checkIfWordInGrid 
    @brief: Given a string and a word search grid, checks if this string
            is located in the grid. Checks if the word is in the grid. Makes use of a 
            recursive auxilliary function to do this.
    @param: String: The word being searched for in the grid.
            WordSearchGrid: The grid that the string is being searched for in.
    @return: A Maybe Placement. Returns orientation of the word if it is contained
             within the grid. Nothing if it is not.
-}
checkIfWordInGrid :: String -> WordSearchGrid -> Maybe Placement
checkIfWordInGrid _ []                                     = Nothing -- string cant be in an empty grid
checkIfWordInGrid [] _                                     = Nothing -- empty string cant be in the grid
checkIfWordInGrid word grid | (maybePlacement == Nothing)  = Nothing --  check didnt find word - word not in grid
                            | otherwise                    = maybePlacement -- check found word, word in grid
    where   
        maybePlacement = (checkIfWordInGridAux word grid (0,0))

{- checkIfWordInGridAux
    @brief: Checks if the given string is withiin the grid. Does this by recursivley going
            through the cells in the grid. If the current cell has the same character as the 
            first character of the given word, it checks if the word starts at this position 
            using a helper function, or it goes onto the next cell if not. If the word is not found
            at this position, it goes onto the next cell, if it is, then it returns the Placement
            structure for this word. If the function reaches the last cell in the grid and doesnt
            find the word, then the word must not be in the grid, and thus it returns 'Nothing'.
    @param: String: The word being searched for.
            WordSearchGrid: The grid being searched in.
            Posn: The position of the current cell being checked.
    @return: A Maybe Placement. The Placement of the word if it is found in the grid. 'Nothing' if not.
-}
checkIfWordInGridAux :: String -> WordSearchGrid -> Posn -> Maybe Placement
checkIfWordInGridAux 
    (l:restOfWord) 
    grid 
    (c, r)              | isOutOfBoundsC1 (c,r) grid = Nothing -- base case, checked all cells (word not in grid)
                        | (grid!!r!!c == l) && (maybePlacementOfWord /= Nothing) = maybePlacementOfWord -- word found at this position
                        | (c+1 < length grid) = checkIfWordInGridAux (l:restOfWord) grid (c+1, r) -- going to check next column
                        | (c+1 == length grid) = checkIfWordInGridAux (l:restOfWord) grid (0, r+1) -- going to check next row
    where
        maybePlacementOfWord = checkIfWordAtPosition (l:restOfWord) (c,r) grid -- checking if the word is found at this position

{- checkIfWordAtPosition
    @brief: Given a string, position (cell) and grid, will check if the string starts at
            this cell (i.e. is the string in the word search grid, starting at the given
            position). Does this by checking all possible orientations for the word using 
            a helper function. If none of them are a match, then the word does not start 
            at this position, and 'Nothing' is returned.
            If a correct orientation for the word is found, then the placement is returned.
    @param: String: Word being searched for.
            Posn: Position where string is believed to start.
            WordSearchGrid: The grid the string is being searched for in.
    @return: Maybe placement. If orientation for the string is found at this position, 
             then returns the orientation of the word. If it is not, returns 'Nothing'.
-}
checkIfWordAtPosition :: String -> Posn -> WordSearchGrid -> Maybe Placement
checkIfWordAtPosition 
    (word) 
    (c,r) 
    grid        | (checkIfWordAtPositionAux word (c,r) forward grid)     = Just ((c,r), Forward)
                | (checkIfWordAtPositionAux word (c,r) back grid)        = Just ((c,r), Back)
                | (checkIfWordAtPositionAux word (c,r) up grid)          = Just ((c,r), Up)
                | (checkIfWordAtPositionAux word (c,r) down grid)        = Just ((c,r), Down)
                | (checkIfWordAtPositionAux word (c,r) upForward grid)   = Just ((c,r), UpForward)
                | (checkIfWordAtPositionAux word (c,r) upBack grid)      = Just ((c,r), UpBack)
                | (checkIfWordAtPositionAux word (c,r) downForward grid) = Just ((c,r), DownForward)
                | (checkIfWordAtPositionAux word (c,r) downBack grid)    = Just ((c,r), DownBack)
                | otherwise                                              = Nothing
    where
        -- vectors for unit movements in orientations
        forward     = (1,0)
        back        = (-1,0)
        up          = (0,-1)
        down        = (0,1)
        upForward   = (1,-1)
        upBack      = (-1,-1)
        downForward = (1,1)
        downBack    = (-1,1)

{-checkIfWordAtPositionAux
    @brief: Recursive function to determine if a given string is found at the given position,
            with the given orientation. The orientation is passed in as a vector representing
            a unit of movement through the grid in direction of this orientation
            Recurses through the letters in the string, checking that the current position of 
            the grid has this letter, and moving to the next cell according to the direction 
            vector. If any letter is not a match, the function returns False. If all letters 
            are checked and matched, the function returns True.
    @param: String: Word being checked.
            Posn: Staring position of the word.
            (Int,Int): The direction vector for the orientation being checked.
            WordSearchGrid: The grid being checked.
    @return: Bool: True if the word is found at the given position with the given orientation,
             False if not.
-}
checkIfWordAtPositionAux :: String -> Posn -> (Int,Int) -> WordSearchGrid -> Bool
checkIfWordAtPositionAux [] _ _ _ = True
checkIfWordAtPositionAux 
    (l:restOfWord)
    currentPosition@(cc,cr)
    direction@(dc,dr)
    grid              | isOutOfBoundsC1 currentPosition grid = False -- gone beyond grid limit
                      | (grid!!cr!!cc /= l)                = False -- letter not a match
                      | (grid!!cr!!cc == l)                = checkIfWordAtPositionAux -- letter matched
                                                                restOfWord 
                                                                (cc+dc, cr+dr) 
                                                                direction 
                                                                grid
{-outOfBounds
    @brief: Given a position and a grid, determines if this position is
            out of bounds of the grid. 
    @param: (Int,Int): The postion being checked.
    @return: True if the given position is out of bounds, False if not.
-}
isOutOfBoundsC1 :: (Int, Int) -> WordSearchGrid -> Bool
isOutOfBoundsC1 position@(c,r) grid |     (c >= length grid) -- outside right
                                     || (r >= length grid) -- outtside bottom
                                     || (c < 0)            -- outside left
                                     || (r < 0) = True     -- outside top
                                  | otherwise   = False



-------------------
-- END OF SOLVER --
-------------------