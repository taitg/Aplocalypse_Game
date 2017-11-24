{- |
Module      : ApocCheck
Description : Contains checks used to determine the winner of the game and to validate strategies
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

module ApocCheck (
    -- * Pawn Conditions
    checkForPawns,
    checkForWhitePawns,
    checkForBlackPawns,
    checkForPawns',
    checkForWhitePawns',
    checkForBlackPawns',
    -- * Penalty Conditions
    checkPenalty,
    -- * Strategy Functions
    getChooser,
    checkLegalStrategy,
    checkLegalStrategy'
    ) where

import ApocTools
import ApocStrategyHuman
import ApocStrategyRandom
import ApocStrategyHostile
import ApocStrategyCoward
import Data.Char (toLower)

---Pawn Conditions-------------------------------------------------

{- | All of the checks used to determine the winner of the game by the number of pawns left on the board. 
-}

-- | Returns Black or White (the winner of the game), or True (draw), or False (continue).
checkForPawns :: GameState -> Either Bool Player
checkForPawns g = do
    let bp = checkForBlackPawns g (4,4)
    let wp = checkForWhitePawns g (4,4)
    if bp && wp
    then Left True
    else if bp
         then Right White
         else if wp
              then Right Black
              else Left False

-- | Returns False if there are White Pawns left on the board, True otherwise.
checkForWhitePawns :: GameState -> (Int, Int) -> Bool
checkForWhitePawns g (-1, y) = checkForWhitePawns g (4, y-1)
checkForWhitePawns g (x, -1) =  True
checkForWhitePawns g (x, y) = if (getFromBoard (theBoard g) (x,y)) == WP
                              then False
                              else checkForWhitePawns g (x-1, y)

-- | Returns False if there are Black Pawns left on the board, True otherwise.
checkForBlackPawns :: GameState -> (Int, Int) -> Bool
checkForBlackPawns g (-1, y) = checkForBlackPawns g (4, y-1)
checkForBlackPawns g (x, -1) =  True
checkForBlackPawns g (x, y) = if (getFromBoard (theBoard g) (x,y)) == BP
                              then False
                              else checkForBlackPawns g (x-1, y)

-- | Returns Black or White (the winner of the game), or Nothing (draw).			  
checkForPawns' :: GameState -> Maybe Player
checkForPawns' g = do
    let bp = checkForBlackPawns' g (4,4) 0
    let wp = checkForWhitePawns' g (4,4) 0
    if bp > wp
    then Just Black
    else if wp > bp
         then Just White
         else Nothing

-- | Returns the number of White Pawns that are left on the board.
checkForWhitePawns' :: GameState -> (Int, Int) -> Int -> Int
checkForWhitePawns' g (-1, y) k = checkForWhitePawns' g (4, y-1) k
checkForWhitePawns' g (x, -1) k = k
checkForWhitePawns' g (x, y)  k = if (getFromBoard (theBoard g) (x,y)) == WP
                                  then checkForWhitePawns' g (x-1, y) k+1
                                  else checkForWhitePawns' g (x-1, y) k

-- | Returns the number of Black Pawns that are left on the board.		  
checkForBlackPawns' :: GameState -> (Int, Int) -> Int -> Int
checkForBlackPawns' g (-1, y) k = checkForBlackPawns' g (4, y-1) k
checkForBlackPawns' g (x, -1) k = k
checkForBlackPawns' g (x, y)  k = if (getFromBoard (theBoard g) (x,y)) == BP
                                  then checkForBlackPawns' g (x-1, y) k+1
                                  else checkForBlackPawns' g (x-1, y) k

---Penalty Conditions----------------------------------------------

{- | The check used to determine the winner of the game by the number of penalties for each player.
-}  

-- | Returns Black or White (the winner of the game), True (draw), False (continue).
checkPenalty :: GameState -> Either Bool Player
checkPenalty g = do
    let bp = blackPen g
    let wp = whitePen g
    if bp == 2 && wp == 2
    then Left True
    else if bp == 2
         then Right White
         else if wp == 2
              then Right Black
              else Left False

---Strategy Functions----------------------------------------------

{- | Functions that are used to validate the strategies entered in the Interactive Mode or by command line arguments.
-}

-- | Takes both strategy names and determines if they are both legal.
checkLegalStrategy :: String -> String -> Bool
checkLegalStrategy stratOne stratTwo = (checkLegalStrategy' stratOne) && (checkLegalStrategy' stratTwo)

-- | Takes a strategy name and checks it against a list to make sure it is legal.
checkLegalStrategy' :: String -> Bool
checkLegalStrategy' strategy = elem (strToLower strategy) ["human", "random", "hostile", "coward"]

-- | Returns the Chooser data type that corresponds to the strategy name given.
getChooser :: String -> Chooser
getChooser strategy | (strToLower strategy) == "human" = human
                    | (strToLower strategy) == "random" = random
                    | (strToLower strategy) == "hostile" = hostile
                    | (strToLower strategy) == "coward" = coward

-- | Takes a string and returns the string in all lowercase
strToLower :: String -> String
strToLower xs = [toLower x | x <- xs]
