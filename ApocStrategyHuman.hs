{- |
Module      : ApocStrategyHuman
Description : Apocalypse's human AI, using CLI input
Copyright   : Copyright 2016, Rumen Kasabov, Travis Paquet, Robert Ranger, Geordie Tait, University of Calgary.
License     : Permission to use, copy, modify, distribute and sell this software
              and its documentation for any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software for any purpose. It is provided "as is" without
              express or implied warranty.
Maintainer  : rkasabov@ucalgary.ca, ttpaquet@ucalgary.ca, rkranger@ucalgary.ca, taitg@ucalgary.ca
Stability   : experimental
Portability : ghc 7.10.2 - 7.10.3

---
This module is used for CPSC 449 for the Apocalypse assignment.

Filled in skeleton.
-}

module ApocStrategyHuman (
    -- * Strategy Function
    human
    ) where

import ApocTools
import Data.Char
--NOTES: 
--type Chooser = GameState -> PlayType -> Player -> IO (Maybe [(Int,Int)])

-- | The 'human' is of type Chooser, so takes a GameState, PlayType, Player and returns an IO (Maybe [(Int,Int)])
--   Prompts the user for a move, and if the input is valid (right quantity of numerical, in range numbers except the comments)
--   then it parses it, and returns it. Otherwise, the user is reprompted until its right.
human :: Chooser
human b Normal c = do
                        --  Printing the prompt
                        putStrLn $ normalPrompt c
                        --  Getting the users input
                        line <- getLine
                        --  If the move is valid, then it's parsed and returned
                        --   otherwise, the method is recursed                       
                        if (validNormal line) then return (humanInputParser line)
                        else human b Normal c

human b PawnPlacement c = do
                            --  Printing the prompt
                            putStrLn $ pawnPrompt c
                            --  Getting the users input
                            line <- getLine
                            --  If the move is valid, then it's parsed and returned
                            --   otherwise, the method is recursed
                            if validPawn line then return (humanInputParser line)
                            else human b PawnPlacement c

-- PROMPTING CODE --

-- | normalPrompt takes a Player, and returns a string
--   Used to generate the prompts for the player when doing a normal turn.
--   Based off the examples provided.
normalPrompt :: Player -> String
normalPrompt c =  "Enter the move coordinates for player \
                  \" ++ (playerToString c) ++ " \
                  \in the form 'srcX srcY destX destY'\n[0 >= n >= 4, or \
                   \just enter return for a 'pass'] " ++ (playerToChar c) ++ "2:\n"

-- | pawnPrompt takes a Player, and returns a string
--   Used to generate the prompts for the player when doing a pawnMovement turn.
--   Based off the examples provided.
pawnPrompt :: Player -> String
pawnPrompt c = "Enter the coordinates to place the pawn for player \
               \" ++ (playerToString c) ++ " in the form \
               \'destX destY'\n[0 >= n >= 4] " ++ (playerToChar c) ++ "1:\n"


-- | playerToChar takes a Player, and returns a one character String representation
playerToChar :: Player -> String
playerToChar Black = "B"
playerToChar White = "W"

-- | playerToString takes a Player, and returns a String representation
playerToString :: Player -> String
playerToString Black = "Black"
playerToString White = "White"

-- | validNormal takes user input [Char] and returns if its a valid move or not
-- Checks for: Length, Range, and if the input provided is actually numbers.
validNormal :: [Char] -> Bool
                --  Check if the length is either 0, or 4
validNormal x | length (words x) == 0 = True
              | not (length( commentRemover x) == 4)  = False
                --  Check that all the remaining characters are numbers
              | False `elem` (map isDigit (commentRemover x)) = False
                --  Check that the numbers are in range
              | not $ inRange (parse' (letters (commentRemover x))) = False
              | otherwise = True

-- | validPawn takes user input [Char] and returns if its a valid move or not
-- Checks for: Length, Range, and if the input provided is actually numbers.
validPawn :: [Char] -> Bool
                --  Check if the length is 2
validPawn x   | length (commentRemover x) == 0 = True
              | not (length( commentRemover x) == 2)  = False
                --  Check that all the remaining characters are numbers
              | False `elem` (map isDigit (commentRemover x)) = False
                --  Check that the numbers are in range
              | not $ inRange (parse' (letters (commentRemover x))) = False
              | otherwise = True

-- | inRange checks to see if a set of integers are within the apocalypse game grid
inRange :: [Int] -> Bool
inRange [] = True
inRange (x:xs) | x < 0 = False
               | x > 4 = False
               | otherwise = inRange xs


-- PARSING CODE --
humanInputParser :: String -> Maybe [(Int,Int)]
humanInputParser x = parse x

-- | commentRemover removes all spaces, and all characters after, and including a "-"
commentRemover :: [Char] -> [Char]
commentRemover [] = []
commentRemover (x:xs) | x == ' ' = commentRemover xs
                      | x /= '-' = x: (commentRemover xs)
                      | otherwise = []

-- | letters, inspired by the built in "words".
-- Returns a [Char] turned into a [[Char]] to facilitate toInt, later on
letters :: [Char] -> [[Char]]
letters [] = []
letters (x:xs) = [x]:(letters xs)

-- | parse is passed the user's input in [Char] form, parses it and returns it as
-- a full fledged valid move, in the form Maybe [(Int,Int)]
-- Can parse Normal, and PawnPlacement moves
parse :: [Char] -> Maybe [(Int,Int)]
parse x | (length (commentRemover x)) == 0 = Nothing
        | otherwise = Just (parse'' $ parse' $ letters $ commentRemover x)

-- | parse'
-- Takes a [[Char]] and returns all the invidual elements as an integer
parse' :: [[Char]] -> [Int]
parse' x = map toInt x
            where
                    toInt x = read x :: Int

-- | parse'' Takes a [Int] and turns it into a [(Int,Int)]
-- Only works if there's an even amount of Ints in [Int].
parse'' :: [Int] -> [(Int,Int)]
parse'' [] = []
parse'' (w:x:xs) = (w,x) : (parse'' xs)