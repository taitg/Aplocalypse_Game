{- |
Module      : ApocStrategyHostile
Description : Module containing hostile AI for playing Apocalypse
              Features a random, but hostile AI.
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
-}

module ApocStrategyHostile (
 -- * Strategy Function
   hostile
   ) where

import ApocTools
import ApocFunctions

import Data.Maybe (fromJust)

-- | Returns a move choice based on the given GameState/PlayType/Player
-- Picks a random piece that the player has control over and makes a move. If the piece
-- can attack, it will. If not a random valid move will be generated.
hostile    :: Chooser
hostile g Normal p = do
    let move = randomMove g (0,0) (0,0) p 0 0
    if move == Nothing then return move
    else return (Just( processTurn g p (fromJust move)))
hostile g PawnPlacement p = return (randomPlace g (2,2) p 0 0)

processTurn :: GameState -> Player -> [(Int,Int)] -> [(Int,Int)]
processTurn g p [(w,x),(y,z)]  = if ((getPieceAt (theBoard g) (w,x)) == WK) || ((getPieceAt (theBoard g) (w,x)) == BK)
                              then [(w,x),(y,z)]
                              else case (p) of 
                                  Black -> checkAllAttacks g p (w,x) (y,z)
                                  White -> checkAllAttacks g p (w,x) (y,z)

-- | CheckAllAttacks, given a specificed GameState, and Player, and location of a piece it will find out if
-- it can attack. If not, it'll just go with the previously decided random move.
checkAllAttacks :: GameState -> Player -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
checkAllAttacks g Black (w,x) (y,z) | (inRange [w-1,x-1]) && (canAttack g (w,x) (w-1,x-1)) = [(w,x),(w-1,x-1)]
                                    | (inRange [w-1,x+1]) && (canAttack g (w,x) (w-1,x+1)) = [(w,x),(w-1,x+1)]
                                    | (inRange [w,x+1]) && (canAttack g (w,x) (w,x+1)) = [(w,x),(w,x+1)]
                                    | otherwise = [(w,x),(y,z)]
checkAllAttacks g White (w,x) (y,z) | (inRange [w+1,x+1]) && (canAttack g (w,x) (w+1,x+1)) = [(w,x),(w+1,x+1)]
                                    | (inRange [w+1,x-1]) && (canAttack g (w,x) (w+1,x-1)) = [(w,x),(w+1,x-1)]
                                    | (inRange [w,x-1]) && (canAttack g (w,x) (w,x-1)) = [(w,x),(w,x-1)]
                                    | otherwise = [(w,x),(y,z)]

-- | canAttack, given a game state, a source, and a destination it will return a boolean
-- describing if an attack is possible or not.
canAttack :: GameState ->  (Int, Int) -> (Int,Int) -> Bool
canAttack g (w,x) (y,z) = if ((getPieceAt (theBoard g) (w,x)) == WP)
                          then (if ((getPieceAt (theBoard g) (y,z)) == BP) then True else False)
                          else (if ((getPieceAt (theBoard g) (y,z)) == WP) then True else False)

-- | inRange returns true if all the numbers in the set
-- are between 0 and 4. (The boundaries of the Apocalypse game board)
inRange :: [Int] -> Bool
inRange [] = True
inRange (x:xs) | x < 0 = False
               | x > 4 = False
               | otherwise = inRange xs

