{- |
Module      : ApocStrategyCoward
Description : Module containing the "coward"  AI strategy for Apocalypse
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

module ApocStrategyCoward (
   coward
   ) where

import ApocTools
import ApocFunctions
import Data.Maybe (fromJust)

-- | Returns a cowardly move choice based on the given GameState, Playtype and Player
coward    :: Chooser
coward g Normal p = return (processTurn g (randomMove g (0,0) (0,0) p 0 0) p)
coward g PawnPlacement p = return (randomPlace g (2,2) p 0 0)

-- | Processes a turn for the cowardly AI. If the player has any knights, pawns will not
--   move and the AI will sometimes pass instead of moving knights. If the player has no
--   knights, pawns will move randomly.
processTurn :: GameState -> Maybe [(Int,Int)] -> Player -> Maybe [(Int,Int)]
processTurn g Nothing p = Nothing
processTurn g move p =
    if (p == Black) &&
       (getPieceAt (theBoard g) ((fromJust move) !! 0) == BP)
    then if getKnights (theBoard g) p > 0
         then Nothing
         else move
    else if (p == White) &&
       (getPieceAt (theBoard g) ((fromJust move) !! 0) == WP)
    then if getKnights (theBoard g) p > 0
         then Nothing
         else move
    else move
