{- |
Module      : ApocFunctions
Description : Module containing utility functions for Apocalypse
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

module ApocFunctions (
      -- * Utility functions
      replace, replace2,
      moveIsValid, placeIsValid,
      movePiece, movePieces, placePiece,
      getPieceAt, getKnights,
      getRandom, getSeed,
      randomMove, randomPlace
      ) where

import Data.Maybe (fromJust, isNothing)
import ApocTools
import Data.Char (ord)
import System.Random

---Utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)

-- | Takes a Board and a "move" and returns the new Board after the move is made
--   (Moves are of type Maybe [(int,Int)])
movePiece       :: Board -> Maybe [(Int,Int)] -> Board
movePiece b Nothing = b
movePiece b move = (replace2 (replace2 (b)
                             ((fromJust move) !! 1)
                             (getFromBoard (b) ((fromJust move) !! 0)))
                   ((fromJust move) !! 0)
                   E)

-- | Takes a Board and two "moves" and returns the resulting Board
--   (Moves are of type Maybe [(int,Int)])
movePieces      :: Board -> Maybe [(Int,Int)] -> Maybe [(Int,Int)] -> Board
movePieces b Nothing Nothing = b
movePieces b blackMove Nothing = movePiece b blackMove
movePieces b Nothing whiteMove = movePiece b whiteMove
movePieces b blackMove whiteMove = do
    -- store pieces that players are trying to move
    let blackPiece = getFromBoard b ((fromJust blackMove) !! 0)
    let whitePiece = getFromBoard b ((fromJust whiteMove) !! 0)
    -- check for clashes
    if ((fromJust blackMove) !! 1) == ((fromJust whiteMove) !! 1)
    then if (blackPiece == BK) && (whitePiece == WP)
         -- black wins the clash, white loses piece
         then replace2 (replace2 (replace2 b
                                           ((fromJust blackMove) !! 1)
                                           blackPiece)
                                 ((fromJust blackMove) !! 0)
                                 E)
                       ((fromJust whiteMove) !! 0)
                       E
         else if (blackPiece == BP) && (whitePiece == WK)
         -- white wins the clash, black loses piece
         then replace2 (replace2 (replace2 b
                                           ((fromJust whiteMove) !! 1)
                                           whitePiece)
                                 ((fromJust blackMove) !! 0)
                                 E)
                       ((fromJust whiteMove) !! 0)
                       E
         -- black and white both lose their pieces in the clash
         else replace2 (replace2 b
                                 ((fromJust blackMove) !! 0)
                                 E)
                       ((fromJust whiteMove) !! 0)
                       E
    -- check if black is moving to piece that white is moving + vice versa
    else if ((fromJust blackMove) !! 1) == ((fromJust whiteMove) !! 0)
    then if ((fromJust whiteMove) !! 1) == ((fromJust blackMove) !! 0)
         then replace2 (replace2 b
                                 ((fromJust blackMove) !! 1)
                                 blackPiece)
                       ((fromJust whiteMove) !! 1)
                       whitePiece
         else replace2 (replace2 (replace2 b
                                           ((fromJust whiteMove) !! 1)
                                           whitePiece)
                                 ((fromJust blackMove) !! 1)
                                 blackPiece)
                       ((fromJust blackMove) !! 0)
                       E
    -- movements don't intersect so just move normally
    else movePiece (movePiece b blackMove) whiteMove

-- | Takes a Board, a "move" and a Player and returns true if valid else false
moveIsValid     :: Board -> Maybe [(Int,Int)] -> Player -> Bool
moveIsValid b Nothing p = True
moveIsValid b move p = case (getPieceAt b ((fromJust move) !! 0)) of
                         BP -> if p /= Black then False
                               else moveIsValid' b
                                                 ((fromJust move) !! 0)
                                                 ((fromJust move) !! 1)
                                                 BP
                         WP -> if p /= White then False
                               else moveIsValid' b
                                                 ((fromJust move) !! 0)
                                                 ((fromJust move) !! 1)
                                                 WP
                         BK -> if p /= Black then False
                               else moveIsValid' b
                                                 ((fromJust move) !! 0)
                                                 ((fromJust move) !! 1)
                                                 BK
                         WK -> if p /= White then False
                               else moveIsValid' b
                                                 ((fromJust move) !! 0)
                                                 ((fromJust move) !! 1)
                                                 WK
                         E  -> False

-- | Helper function for moveIsValid
moveIsValid'    :: Board -> (Int,Int) -> (Int,Int) -> Cell -> Bool
moveIsValid' b fromXY toXY c =
    case c of
      BP -> if ((fst toXY)==(fst fromXY)) &&
               ((snd toXY)==(snd fromXY)-1) &&
               ((getPieceAt b toXY)==E)
            then True
            else if ((fst toXY)==(fst fromXY)-1) &&
                    ((snd toXY)==(snd fromXY)-1) &&
                    (((getPieceAt b toXY)==WP) ||
                    ((getPieceAt b toXY)==WK))
            then True
            else if ((fst toXY)==(fst fromXY)+1) &&
                    ((snd toXY)==(snd fromXY)-1) &&
                    (((getPieceAt b toXY)==WP) ||
                    ((getPieceAt b toXY)==WK))
            then True
            else False
      WP -> if ((fst toXY)==(fst fromXY)) &&
               ((snd toXY)==(snd fromXY)+1) &&
               ((getPieceAt b toXY)==E)
            then True
            else if ((fst toXY)==(fst fromXY)-1) &&
                    ((snd toXY)==(snd fromXY)+1) &&
                    (((getPieceAt b toXY)==BP) ||
                    ((getPieceAt b toXY)==BK))
            then True
            else if ((fst toXY)==(fst fromXY)+1) &&
                    ((snd toXY)==(snd fromXY)+1) &&
                    (((getPieceAt b toXY)==BP) ||
                    ((getPieceAt b toXY)==BK))
            then True
            else False
      BK -> if (elem ((fst fromXY)-(fst toXY),
                      (snd fromXY)-(snd toXY))
                     [(1,2),(1,-2),(-1,2),(-1,-2),
                      (2,1),(2,-1),(-2,1),(-2,-1)]) &&
               ((getPieceAt b toXY)/=BK) &&
               ((getPieceAt b toXY)/=BP)
            then True
            else False
      WK -> if (elem ((fst fromXY)-(fst toXY),
                      (snd fromXY)-(snd toXY))
                     [(1,2),(1,-2),(-1,2),(-1,-2),
                      (2,1),(2,-1),(-2,1),(-2,-1)]) &&
               ((getPieceAt b toXY)/=WK) &&
               ((getPieceAt b toXY)/=WP)
            then True
            else False

-- | Takes a Board and an (x,y) and returns the Cell at those coordinates
getPieceAt      :: Board -> (Int,Int) -> Cell
getPieceAt b (x,y) = b !! y !! x

-- | Takes a Board and an (x,y) and returns true if placement is valid, else false
placeIsValid    :: Board -> (Int,Int) -> Bool
placeIsValid b coords = if (getPieceAt b coords)==E then True
                        else False

-- | Takes a Board and a Player and returns how many knights they have
getKnights      :: Board -> Player -> Int
getKnights b p = case p of
                   Black -> getCount (foldr (++) [] b) BK 0
                   White -> getCount (foldr (++) [] b) WK 0

-- | Helper function for getKnights
getCount        :: [Cell] -> Cell -> Int -> Int
getCount [] target count = count
getCount (c:cs) target count = if c==target then (getCount cs target (count+1))
                                  else (getCount cs target count)

-- | Takes a Board, initial/final (x,y) and a Bool (true = upgrade to knight,
--   false = pawn placement) and returns the new Board after pawn placement
placePiece      :: Board -> (Int,Int) -> (Int,Int) -> Bool -> Board
placePiece b (-1,-1) toXY upgrade = b
placePiece b fromXY (-1,-1) False = b
placePiece b fromXY toXY upgrade = if not upgrade
                                   then movePiece b (Just [fromXY,toXY])
                                   else if (getPieceAt b fromXY)==BP && (snd fromXY) == 0
                                        then replace2 b fromXY BK
                                   else if (getPieceAt b fromXY)==WP && (snd fromXY) == 4
                                        then replace2 b fromXY WK
                                   else b

-- | Makes a list of random int from low to high using seed, returns the int at index
getRandom       :: Int -> Int -> Int -> Int -> Int
getRandom seed index low high = if index > 0
                                then (randomRs (low,high) (mkStdGen seed)) !! index
                                else 0

-- | Takes GameState and generates an int to use as a seed
getSeed         :: GameState -> Int
getSeed g = (((strToNum (board2Str ([(theBoard g) !! 0]))) *
              (strToNum (board2Str ([(theBoard g) !! 1]))) *
              (strToNum (board2Str ([(theBoard g) !! 2]))) +
              (strToNum (board2Str ([(theBoard g) !! 3]))) *
              (strToNum (board2Str ([(theBoard g) !! 4])))) `quot`
              (strToNum (show (blackPlay g))+
              ((blackPen g)+1)+
              strToNum (show (whitePlay g))+
              ((whitePen g)+1)))

-- | Generates an integer from a given string
strToNum        :: String -> Int
strToNum [] = 0
strToNum (x:xs) = (ord x) + (strToNum xs)

-- | Checks if the given move is valid, else tries again with new random coordinates.
--   Passes after 1000 failed attempts to find a valid move
randomMove :: GameState -> (Int,Int) -> (Int,Int) -> Player -> Int -> Int -> Maybe [(Int,Int)]
randomMove g fromXY toXY p prev attempts =
    if attempts > 1000 then Nothing
    else if (moveIsValid (theBoard g) (Just [fromXY,toXY]) p)
         then if (getRandom (getSeed g) 0 0 100) > 99
              then Nothing
              else Just ([fromXY,toXY])
         else randomMove g
                         ((getRandom (getSeed g) prev 0 4),
                          (getRandom (getSeed g) (prev+1) 0 4))
                         ((getRandom (getSeed g) (prev+2) 0 4),
                          (getRandom (getSeed g) (prev+3) 0 4))
                         p
                         (prev+4)
                         (attempts+1)

-- | Checks if the given place is valid, else tries again with new random coordinates.
--   Passes after 1000 attempts to find a valid pawn placement
randomPlace :: GameState -> (Int,Int) -> Player -> Int -> Int -> Maybe [(Int,Int)]
randomPlace g coords p prev attempts =
    if attempts > 1000 then Nothing
    else if (placeIsValid (theBoard g) coords)
         then Just ([coords])
         else randomPlace g
                          ((getRandom (getSeed g) prev 0 4),
                           (getRandom (getSeed g) (prev+1) 1 3))
                          p
                          (prev+2)
                          (attempts+1)

