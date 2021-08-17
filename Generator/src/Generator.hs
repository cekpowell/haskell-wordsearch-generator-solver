-- COMP2209 Functional Programming Challenges
-- Author: Charles Powell
-- (c) University of Southampton 2020

module Generator
    ( WordSearchGrid,Placement,Posn,Orientation(..),
    createWordSearch
    ) where


-----------------------
-- IMPORT STATEMENTS -- 
-----------------------


import System.Random
import Data.Char


-----------------------
-- DATA DEFINITIONS  -- 
-----------------------


type WordSearchGrid = [[ Char ]]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read)


------------------------
-- START OF GENERATOR -- 
------------------------


--------------------
-- MAIN FUNCTIONS --
-------------------- 

{- createWordSearch
    @brief: Main function for the challenge. Given a list of words and 
            a maximum density, returns a word search grid containing theese
            words in a random position and orientation, with a density less
            than the given maximum density.

            Main logic:
                - Use the list of words and maximum density to find the smallest
                  needed grid size.
                - Create an empty grid of this dimension, with blank ('~') 
                  characters in every position.
                - Recurse through the words and for each word:
                    - Generate a random position.
                    - Generate a random orientation.
                    - See if the word can be placed in the current grid with this
                      placement.
                    - If it can, place it in the grid, and move onto the next word.
                    - If it cant, make another random placement and try to place again.
                    - If a word has a large number of failed attempts, assume it can't be
                      placed in the grid. Thus, a larger grid is needed for the words.
                    - Make a larger blank grid, and start from the first word again.
                - When all words are added, fill in the remainder of the blank symbols
                  with random characters from the input words.
                - Return the completed grid.
    @param: [String] : The words to be inserted into the wordsearch.
            Double : The max density of the grid to be made.
    @return: IO WordSearchGrid : The resulting grid made in the process.
             Error : If density is equal to 1 or 0 (such grids are not possible)
-}
createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch words maxDensity | (maxDensity <=0) || (maxDensity >1) = error "Invalid max density."
                                  | words == []                         = return []
                                  | otherwise = createWordSearchAux uWords newEmptyGrid uWords
    where 
        newEmptyGrid = getEmptyWordSearchGrid (getMinimumGridSize words maxDensity)
        uWords       = map (map toUpper) words
         

{- createWordSearchAux
    @brief: Auxilliary function to create a word search. Given a list of words 
            and an empty word search grid, recurses through the list of words, 
            finding a placement for each word and then adding it to the grid.
            If a word cant be placed, the function resets with all of the words
            and a bigger grid.
    @param: [String]: The set of words to be added to the grid.
            WordSearchGrid: The empty word search grid of the needed dimension.
            [String]: The set of words to be added (not reduced under recursion
            so that the function can be reset).
    @return: IO WordSearchGrid: The generated grid containing the given words.
             Error: If one of the words is the empty string (""). Such a word
             cannot be added to a grid.
-}
createWordSearchAux :: [String] -> WordSearchGrid -> [String] -> IO WordSearchGrid
createWordSearchAux [] grid allWords = do
                                        let uniqueCharacters = [x | y <- allWords, x <- y]
                                        filledInGrid <- fillBlanks uniqueCharacters grid
                                        return filledInGrid
createWordSearchAux 
    (w:restOfWords) 
    grid 
    allWords         | w == []   = error "Invalid input word: Words length must be >= 1." -- cant insert empty word into a grid
                     | otherwise =  do 
                                        placement <- (getRandomPlacement w grid 0)
                                        let cantPlace = (placement == ((0,0),Up))
                                        if cantPlace then  -- word cant be placed - bigger grid needed
                                            let newBiggerDimension = floor (fromIntegral(length grid)*1.5) in
                                            let newBiggerGrid = (getEmptyWordSearchGrid newBiggerDimension) in
                                            createWordSearchAux allWords newBiggerGrid allWords
                                        else
                                            let updatedGrid = addWord w placement grid in 
                                            createWordSearchAux restOfWords updatedGrid allWords

--------------------------------------
-- GETTING NEEDED DIMENSION OF GRID -- 
--------------------------------------

{- getMinimumGridSize
    @brief: Given the set of words to be added and the max density of the grid,
            the function will calculuate the minimum dimension of the grid.
            The dimension of the grid must be large enough that the ration between
            the characters in the words and the filler characters is less than the 
            max density. Also, the grid must be large enough to fit the largest word in
            the set into it. Thus, the minimum grid dimension will be the max of the 
            length of the largest word or the dimension according to the density.
    @param:[String]: words to be added to the grid.
           Double: Max density of the grid.
    @return: Int: The minimum dimension of a grid to satisfy the constraints.
-}
getMinimumGridSize :: [String] -> Double -> Int
getMinimumGridSize words maxDensity = max (gridSizeByDensity) (gridSizeByWordLength)
    where   
        gridSizeByDensity    = getMinimumGridSizeByDensity words maxDensity
        gridSizeByWordLength = getMinimumGridSizeByWordLength words

{- getMinimumGridSizeByDensity
    @brief: Given a a set of words, and a max density, the function will calculute the 
            dimension of the smallest grid that can will have a density less than the max
            density. The minimum grid size found by density is an nxn grid where nxn is 
            the next square number after (number of characters in words)/maxDensity. 
            A grid of this size will always have a density less than the given maximum 
            density.
    @param: [String]: Set of words to be added.
            Double: Max density of grid.
    @return: Int: Minimum dimension of the grid to ensure density is less than max density.
-}
getMinimumGridSizeByDensity :: [String] -> Double -> Int
getMinimumGridSizeByDensity words maxDensity = nextSquareNumber minimumCellsNeeded 0
    where   
        totalCharacters    = length [x | y <- words, x <- y]
        minimumCellsNeeded = ceiling (fromIntegral(totalCharacters) / maxDensity)

{- nextSquareNumber
    @brief: Helper function to calculate the minimum dimension according to density.
            Recursivley finds the root of the next square number after the given number.
    @param: Int: The number for which we want to find the next square number.
            Int: The current guess for the root of the next square number.
    @return: Int: The root of the next square number after the input number.
-}
nextSquareNumber :: Int -> Int -> Int
nextSquareNumber number currentGuess | (currentGuess*currentGuess) > number = currentGuess
                                     | otherwise                            = nextSquareNumber 
                                                                                number 
                                                                                (currentGuess+1)

{- getMinimumGridSizeByWordLength
    @brief: Given a set of words, calculates minimum dimension for a grid to contain
            all of these words. The minimum grid size found by length is a grid nxn 
            where n is the length of the longest word in the given list. Thus function 
            finds the longest word by recursivley going through each word and seeing if 
            its length is greater than the current length.
    @param: [String]: The set of words.
    @return: Int: The length of the largest word.
-}
getMinimumGridSizeByWordLength :: [String] -> Int
getMinimumGridSizeByWordLength words = getMinimumGridSizeByWordLengthAux words 0

{- getMinimumGridSizeByWordLengthAux
    @brief: Auxilliary function for 'getMinimumGridSizeByWordLength'. Recursivley
            finds the length of the largest word in the list of words.
    @param: [String]: The list of words.
            Int: Length of the current largest word.
    @return: Int: Length of the largest word in the list.
-}
getMinimumGridSizeByWordLengthAux :: [String] -> Int -> Int
getMinimumGridSizeByWordLengthAux [] currentMaxWordLength        = currentMaxWordLength
getMinimumGridSizeByWordLengthAux (w:words) currentMaxWordLength = getMinimumGridSizeByWordLengthAux 
                                                                        words 
                                                                        (max (length w) 
                                                                             (currentMaxWordLength))

-------------------------
-- CREATING EMPTY GRID --
-------------------------

{- getEmptyWordSearchGrid
    @brief: Given a dimension ,n, creates an empty n x n grid with this dimension.
            This is a list of n items, each of which contains n copies of '~'. These
            are a special character used to signify that no letter has been placed at 
            this position in the grid yet (used when finding a placement for a word.)
    @param: Int: The dimension of the grid.
    @return: WordSearchGrid: A grid of the specified dimension, with every position
             set to '~'.
-}
getEmptyWordSearchGrid :: Int -> WordSearchGrid
getEmptyWordSearchGrid n = replicate n (concat (replicate n "~"))

-----------------------------------------
-- FINDING RANDOM PLACEMENT FOR A WORD -- 
-----------------------------------------

{- getRandomPlacement
    @brief: Given a word, a word search grid, and a counter, attempts
            to find a random placement for the word within the grid.
            Main logic:
                - Use helper functions to generate a random position and random
                  orientation for the word.
                - Use helper functions to check if this placement of the word in 
                  the grid is valid.
                - If it is, return the placement.
                - If it is not, recall the function so that another random placement
                  is tried.
                - A counter records number of times the function has attempted
                  to find a placement for this word. If the counter is much larger
                  than (n x n), the chances are that the word cannot be placed.
                - In this case, return an INVALID placement to signify the word
                  cannot be palced in the grid, and the calling function will create a
                  new grid.
    @param: String: The word for which a placement is being seeked.
            WordSearchGrid: The grid we are trying to place the word into.
    @return: A placement if one was found. If no placement was found, returns 
             ((0,0), Nothing), which is an invalid placement.
-}
getRandomPlacement :: String -> WordSearchGrid -> Int-> IO Placement
getRandomPlacement word grid attempts | (attempts >= (length grid) * (length grid) * 4) = return ((0,0),Up)
                                      | otherwise = 
                                            do 
                                                orientation <- getRandomOrientation
                                                position <- getRandomPosition (length grid)
                                                let placement = (position, orientation)
                                                let correct = isCorrectPlacement word placement grid 
                                                if correct then 
                                                    return placement
                                                else 
                                                    (getRandomPlacement  word grid (attempts+1))
{- getRandomPosition
    @brief: Given the dimension of a grid, returns a random position within
            the grid.
    @param: Int: Dimension of the grid.
    @return: IO Posn: The randomly generated position within the grid.
-}
getRandomPosition :: Int -> IO Posn
getRandomPosition dimension = do 
                                c <- randomRIO (0, dimension - 1)
                                r <- randomRIO (0, dimension - 1)
                                return (c,r)

{- getRandomOrientation
    @brief: Returns a random orientation by generating a random number and indexing
            the list of all orientations.
    @param: N/A
    @return: IO Orientation: A randomly generated orientation
-}
getRandomOrientation :: IO Orientation
getRandomOrientation = fmap (orientations!!) (randomRIO (0,((length orientations)-1)))
    where
        orientations = [Forward, Back, Up, Down, UpForward, UpBack, DownForward, DownBack]

{- isCorrectPlacement
    @brief: Given a word, placement and grid, will verify if the word can be placed in the 
            grid at this position. This is done by checking pattern matching the orientation
            of the word, and using a helper function to check if the word can be placed in 
            the given direction at the given position. The orientation is passed in as a vector
            representing one unit of movement in the given orientation.
    @param: String: Word being verified.
            Placement: The placement being verified.
            WordSearchGrid: The grid the word is to be placed in.
    @return: Bool: True if the word can be placed, False if not.
-}
isCorrectPlacement :: String -> Placement -> WordSearchGrid -> Bool
isCorrectPlacement 
    (word) 
    ((c,r), orientation) 
    grid                  | orientation == Forward     = isCorrectPlacementAux word (c,r) forward grid
                          | orientation == Back        = isCorrectPlacementAux word (c,r) back grid
                          | orientation == Up          = isCorrectPlacementAux word (c,r) up grid
                          | orientation == Down        = isCorrectPlacementAux word (c,r) down grid
                          | orientation == UpForward   = isCorrectPlacementAux word (c,r) upForward grid
                          | orientation == UpBack      = isCorrectPlacementAux word (c,r) upBack grid
                          | orientation == DownForward = isCorrectPlacementAux word (c,r) downForward grid
                          | orientation == DownBack    = isCorrectPlacementAux word (c,r) downBack grid
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

{- isCorrectPlacementAux
    @brief: Given a word, position, orientation vector and grid, determines if the word
            can be placed at the given position, with the given direction. Does this by
            recursing through the letters of the word, and breaking if at any point there
            is a block in the path, or if the word goes beyong the limits of the grid.
            A letter can be placed at the current position if the current position is
            the same letter as it, or if it is a '~', signifying that a letter has not
            yet been placed at this position.
    @param: String: Word being verified.
            Posn: Position the word is being placed at.
            (Int, Int): Directiion vector for the orientation of the word.
            WordSearchGrid: The grid the word is to be placed in.
    @return: Bool: True if the placement is valid, False if not.
-}
isCorrectPlacementAux :: String -> Posn -> (Int,Int) -> WordSearchGrid -> Bool
isCorrectPlacementAux [] _ _ _ = True
isCorrectPlacementAux 
    (l:restOfWord)
    currentPosition@(cc,cr)
    direction@(dc,dr)
    grid              | isOutOfBoundsC2 currentPosition grid = False 
                      | (grid!!cr!!(cc) /= '~') && (grid!!cr!!(cc) /= l) = False -- this cell isnt free, and isnt a  match
                      | (grid!!cr!!(cc) == '~') || (grid!!cr!!(cc) == l) = isCorrectPlacementAux 
                                                                            restOfWord 
                                                                            (cc+dc, cr+dr) 
                                                                            direction grid-- this cell is free or is a  match
{-outOfBoundsC2
    @brief: Given a position and a grid, determines if this position is
            out of bounds of the grid. 
    @param: (Int,Int): The postion being checked.
    @return: True if the given position is out of bounds, False if not.
-}
isOutOfBoundsC2 :: (Int, Int) -> WordSearchGrid -> Bool
isOutOfBoundsC2 position@(c,r) grid |     (c >= length grid) 
                                     || (r >= length grid)
                                     || (c < 0) 
                                     || (r < 0) = True
                                  | otherwise   = False

-------------------
-- ADDING A WORD -- 
-------------------

{- addWord
    @brief: Given a word, a placement and a grid, will place the word into the grid.
            This is done by pattern matching the orientation of the placement, and 
            calling a helper function to place the word into the grid in the given 
            orientation. The orientation is passed in as a vector representing a unit
            of movement in the direction of the  orientation.
    @param: String: Word being placed in the grid.
            Placement: The placement of the word.
            WordSearchGrid: The grid the word is being inserted into.
    @return: WordSearchGrid: Returns the input wordsearch grid, with the word added.
-}
addWord :: String -> Placement -> WordSearchGrid -> WordSearchGrid
addWord 
    word 
    placement@((c,r), orientation) 
    grid                               | orientation == Forward     = addWordAux word (c,r) forward grid
                                       | orientation == Back        = addWordAux word (c,r) back grid
                                       | orientation == Up          = addWordAux word (c,r) up grid
                                       | orientation == Down        = addWordAux word (c,r) down grid
                                       | orientation == UpForward   = addWordAux word (c,r) upForward grid
                                       | orientation == UpBack      = addWordAux word (c,r) upBack grid
                                       | orientation == DownForward = addWordAux word (c,r) downForward grid
                                       | orientation == DownBack    = addWordAux word (c,r) downBack grid
    where
        -- vectors for unit movement in orientations
        forward     = (1,0)
        back        = (-1,0)
        up          = (0,-1)
        down        = (0,1)
        upForward   = (1,-1)
        upBack      = (-1,-1)
        downForward = (1,1)
        downBack    = (-1,1)


{- addWordAux
    @brief: Given a word, position of placement, direction vector and grid, will place
            the word into the grid at this position in the orientation dictated by the 
            direction vector. This is done by recursing through the letters in the word,
            and calling a helper function to add each letter of the word individually.
    @param: String: The word being added to the grid.
            Posn: The position where the word must be added.
            (Int,Int): The direction vector representing the orientation of the word.
            WordSearchGrid: The grid the word is being inserted into.
    @return: WordSearchGrid: The input grid with the given word inserted.
-}
addWordAux :: String -> Posn -> (Int,Int) -> WordSearchGrid -> WordSearchGrid
addWordAux [] _ _ grid      = grid
addWordAux 
    (l:word) 
    currentPosition@(cc,cr) 
    direction@(dc,dr) 
    grid                    = addWordAux word (cc+dc,cr+dr) direction (addLetter l grid currentPosition)

{- addLetter
    @brief: Given a letter, grid, and position, will add the letter into this positiion 
            of the grid. Does this by recursing through the rows of the grid until in the 
            needed row, and then calling a helper function to add the letter into the
            needed column of this row.
    @param: Char: The letter being added.
            WordSearchGrid: The grid the letter is being added into.
            Posn: The position the letter is to be insserted into.
    @return: WordSearchGrid: The input grid with the letter inserted at the given position.
-}
addLetter :: Char -> WordSearchGrid -> Posn -> WordSearchGrid
addLetter _ [] _ = []
addLetter l (row:restOfGrid) position@(c,r) | r == 0    = (addLetterAux l row c):restOfGrid
                                            | otherwise = row:(addLetter l restOfGrid (c,r-1))

{- addLetterAux
    @brief: Given a letter, a row within a grid, and a column number, will insert the 
            letter into this column of the row. Does this by recursing through the columns
            in the row until it finds the needed column.
    @param: Char: The letter being inserted into the grid.
            [Char]: The row the letter is to be inserted into
            Int: The column number the letter is to be inserted at.
    @return: [Char]: The input row with the input letter at the input column number.
-}
addLetterAux :: Char -> [Char] -> Int -> [Char]
addLetterAux _ [] _                 = []
addLetterAux l (x:xs) c | c == 0    = l:xs
                        | otherwise = x:(addLetterAux l xs (c-1))

---------------------------
-- FILLING IN THE BLANKS --
---------------------------

{- fillBlanks
    @brief: Given the set of characters in all words and the wordsearch grid, will
            replace all of the blank ('~) characters within the grid with a random
            character from the set of characters. This is done by recursing through 
            the grid and calling a helper function on each row within the grid.
    @param: [Char]: The set of all characters within the input words.
            WordSearchGrid: The grid for which the blanks need to be filled.
    @return: IO WordSearchGrid: The input grid with all of the blanks replaced with
             random characters.
-}
fillBlanks :: [Char] -> WordSearchGrid -> IO WordSearchGrid
fillBlanks characters []               = return [] 
fillBlanks characters (row:restOfGrid) = do  
                                            newRow <- fillBlanksAux characters row
                                            remainder <- fillBlanks characters restOfGrid
                                            return (newRow:remainder)

{- fillBlanksAux
    @brief: Given the set of characters, and a row from a grid, will replace
            all '~' within the grid with a random character, taken from the 
            list of input characteres. This is done by recursing through the 
            row and checking if the character at this position is a '~' or not.
    @param: [Char]: The set of characters within the input words.
            [Char]: The row for which the blanks must be replaced.
    @return: IO [Char]: The input row with all of the '~' replaced with a random
             character from the input list.
-}
fillBlanksAux :: [Char] -> [Char] -> IO [Char]
fillBlanksAux characters []                         = return []
fillBlanksAux characters (l:restOfRow) | (l == '~') = do        
                                                        randomChar <- (getRandomChar characters)
                                                        remainder <- fillBlanksAux characters restOfRow
                                                        return (randomChar:remainder)
                                       | otherwise  = do
                                                        remainder <- fillBlanksAux characters restOfRow
                                                        return (l:remainder)

{- getRandomChar
    @brief: Given a list of characters, returns a random character from this list.
    @param: [Char]: The list of characters.
    @return: IO Char: A random character from the input list.
-}
getRandomChar :: [Char] -> IO Char
getRandomChar characters = do
                            randomIndex <- randomRIO(0,length characters-1)
                            return (characters!!randomIndex)



----------------------
-- END OF GENERATOR -- 
----------------------