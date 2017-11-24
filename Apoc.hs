{- |
Module      : Main
Description : Main module for a program which plays the game Apocalypse (CPSC 449 Assignment 1)
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

module Main (
      -- * Main
      main, main'
      ) where

import Data.Maybe (fromJust, isNothing)
import System.Environment
import System.IO.Unsafe
import ApocTools
import ApocStrategyHuman
import ApocStrategyCoward
import ApocFunctions
import ApocStrategyRandom
import ApocCheck

---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:

     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
main'           :: [String] -> IO()
main' args = do
    if (length args) == 2
    then if checkLegalStrategy (head args) (last args)
         then startGame (head args) (last args)
         else putStr "  human\n  random\n  hostile\n  coward\n"
    else if (length args) == 0
         then promptForStrategy
         else error "Error: Incorrect number of strategies given"

-- | The primary repeating function which manages each round of gameplay. Takes a GameState
--   and 2 strategy names (type String) and upon completion either calls itself,
--   'pawnPlacement' or 'gameOver'
gameLoop :: GameState -> String -> String -> IO()
gameLoop oldGameState blackStrat whiteStrat = do

    -- get moves from strategies
    blackMove <- (getChooser blackStrat) oldGameState Normal Black
    whiteMove <- (getChooser whiteStrat) oldGameState Normal White

    -- check if moves are valid
    let blackMoveValid = moveIsValid (theBoard oldGameState)
                                     blackMove
                                     Black

    let whiteMoveValid = moveIsValid (theBoard oldGameState)
                                     whiteMove
                                     White

    -- determine the "Played" states
    let blackPlayed = if blackMove==Nothing then Passed
                      else if blackMoveValid
                           then Played (head (fromJust blackMove),
                                        head (tail (fromJust blackMove)))
                           else Goofed (head (fromJust blackMove),
                                        head (tail (fromJust blackMove)))

    let whitePlayed = if whiteMove==Nothing then Passed
                      else if whiteMoveValid
                           then Played (head (fromJust whiteMove),
                                        head (tail (fromJust whiteMove)))
                           else Goofed (head (fromJust whiteMove),
                                        head (tail (fromJust whiteMove)))

    -- add penalties if there are any
    let newBlackPen = (blackPen oldGameState) +
                      (if blackMoveValid then 0 else 1)

    let newWhitePen = (whitePen oldGameState) +
                      (if whiteMoveValid then 0 else 1)

    -- construct the new board from the moves
    let newBoard = movePieces (theBoard oldGameState)
                              (if blackMoveValid then blackMove
                               else Nothing)
                              (if whiteMoveValid then whiteMove
                               else Nothing)

    -- construct the new game state
    let newGameState = GameState blackPlayed
                                 newBlackPen
                                 whitePlayed
                                 newWhitePen
                                 newBoard

    -- print the new game state
    putStrLn $ show newGameState

    -- check if pawn placement happens
    let blackPlacesPawn = if blackMove == Nothing then False
                          else if blackMoveValid &&
                             (snd ((fromJust blackMove) !! 1) == 0) &&
                             (getPieceAt newBoard ((fromJust blackMove) !! 1) == BP)
                          then True
                          else False

    let whitePlacesPawn = if whiteMove == Nothing then False
                          else if whiteMoveValid &&
                             (snd ((fromJust whiteMove) !! 1) == 4) &&
                             (getPieceAt newBoard ((fromJust whiteMove) !! 1) == WP)
                          then True
                          else False

    -- check if the game is over
    let gameIsOver = if (newBlackPen==2) || (newWhitePen==2) then True
                 else if (blackMove==Nothing) && (whiteMove==Nothing) then True
                 else if (checkForPawns newGameState /= (Left False)) then True
                 else False

    -- end the game, go to pawn placement or restart game loop with new game state
    if gameIsOver then gameOver newGameState blackStrat whiteStrat
    else if blackPlacesPawn || whitePlacesPawn
         then pawnPlacement newGameState
                            blackStrat blackPlacesPawn (if blackMove == Nothing then (-1,-1)
                                                        else ((fromJust blackMove) !! 1))
                            whiteStrat whitePlacesPawn (if whiteMove == Nothing then (-1,-1)
                                                        else ((fromJust whiteMove) !! 1))
         else gameLoop newGameState blackStrat whiteStrat

-- | The function for handling pawn placement rounds - acts as a modified version of the gameloop.
--   Takes a GameState and for each player: a strategy name (String), a Bool (true if player is
--   allowed to place a pawn) and an (x,y) coordinate representing the location of the pawn to be
--   moved or upgraded. Upon completion, it calls 'gameLoop' or 'gameOver'
pawnPlacement :: GameState ->
                 String -> Bool -> (Int,Int) ->
                 String -> Bool -> (Int,Int) ->
                 IO()
pawnPlacement oldGameState
              blackStrat blackCanPlace blackXY
              whiteStrat whiteCanPlace whiteXY = do

    -- check if pawn should upgrade to knight
    let blackUpgrade = if (getKnights (theBoard oldGameState) Black) < 2
                       then True
                       else False

    let whiteUpgrade = if (getKnights (theBoard oldGameState) White) < 2
                       then True
                       else False

    -- get pawn placement moves
    blackPlace <- if blackCanPlace && (not blackUpgrade)
                  then (getChooser blackStrat) oldGameState PawnPlacement Black
                  else return Nothing
    whitePlace <- if whiteCanPlace && (not whiteUpgrade)
                  then (getChooser whiteStrat) oldGameState PawnPlacement White
                  else return Nothing


    -- check if pawn placements are valid
    let blackPlaceValid = if blackPlace == Nothing then True
                          else if not blackUpgrade
                          then placeIsValid (theBoard oldGameState)
                                            (head (fromJust blackPlace))
                          else True

    let whitePlaceValid = if whitePlace == Nothing then True
                          else if not whiteUpgrade
                          then placeIsValid (theBoard oldGameState)
                                            (head (fromJust whitePlace))
                          else True

    -- determine the "Played" states
    let blackPlayed = if not blackCanPlace then None
                      else if blackUpgrade then UpgradedPawn2Knight blackXY
                      else if blackPlace==Nothing then NullPlacedPawn
                      else if not blackPlaceValid
                           then BadPlacedPawn (blackXY,
                                              (head (fromJust blackPlace)))
                      else PlacedPawn (blackXY,
                                      (head (fromJust blackPlace)))

    let whitePlayed = if not whiteCanPlace then None
                      else if whiteUpgrade then UpgradedPawn2Knight whiteXY
                      else if whitePlace==Nothing then NullPlacedPawn
                      else if not whitePlaceValid
                           then BadPlacedPawn (whiteXY,
                                              (head (fromJust whitePlace)))
                      else PlacedPawn (whiteXY,
                                      (head (fromJust whitePlace)))

    -- update penalties if there are any
    let newBlackPen = (blackPen oldGameState) +
                      (if blackPlaceValid && (blackPlayed /= NullPlacedPawn)
                       then 0 else 1)

    let newWhitePen = (whitePen oldGameState) +
                      (if whitePlaceValid && (whitePlayed /= NullPlacedPawn)
                       then 0 else 1)

    -- construct the new board
    let newBoard = placePiece (placePiece (theBoard oldGameState)
                                          (if blackPlaceValid
                                           then blackXY
                                           else (-1,-1))
                                          (if (blackPlace == Nothing) ||
                                              (blackPlayed == None)
                                           then (-1,-1)
                                           else head (fromJust blackPlace))
                                          blackUpgrade)
                              (if whitePlaceValid
                               then whiteXY
                               else (-1,-1))
                              (if (whitePlace == Nothing) ||
                                  (whitePlayed == None)
                               then (-1,-1)
                               else head (fromJust whitePlace))
                              whiteUpgrade

    -- construct the new game state
    let newGameState = GameState blackPlayed
                                 newBlackPen
                                 whitePlayed
                                 newWhitePen
                                 newBoard

    -- print the new game state
    putStrLn $ show newGameState

    -- check if the game is over
    let gameIsOver = if (newBlackPen==2) || (newWhitePen==2) then True
                 else if (checkForPawns newGameState /= (Left False)) then True
                 else False

    -- if the game is over then end it, else restart game loop with new state
    if gameIsOver then gameOver newGameState blackStrat whiteStrat
    else gameLoop newGameState blackStrat whiteStrat

-- | The function which handles the end of the game and determines the winner. Takes a
--   GameState and two strategy names (type String) and prints the name of the winner
--   as well as the number of pawns for each player/strategy. 
gameOver        :: GameState -> String -> String -> IO()
gameOver finalGameState bStrat wStrat= do
    let pen = checkPenalty finalGameState
    let pawn = checkForPawns finalGameState
    let pawn' = checkForPawns' finalGameState
    let bpawn = checkForBlackPawns' finalGameState (4, 4) 0
    let wpawn = checkForWhitePawns' finalGameState (4, 4) 0

    if (pen /= (Left True)) || (pen /= (Left False))
    then if (pen == (Right White))
         then putStrLn ("White wins! (Because Black accumulated >1 penalty points.)  Black (" ++ bStrat ++ "): " ++ show(bpawn) ++ "  White (" ++ wStrat ++ "): " ++ show(wpawn))
         else if (pen == (Right Black))
              then putStrLn ("Black wins! (Because White accumulated >1 penalty points.)  Black (" ++ bStrat ++ "): " ++ show(bpawn) ++ "  White (" ++ wStrat ++ "): " ++ show(wpawn))
              else if (pawn /= (Left True)) || (pawn /= (Left False))
                   then if (pawn == (Right White))
                        then putStrLn ("White wins! Black (" ++ bStrat ++ "): " ++ show(bpawn) ++ "  White (" ++ wStrat ++ "): " ++ show(wpawn))
                        else if (pawn == (Right Black))
                             then putStrLn ("Black wins! Black (" ++ bStrat ++ "): " ++ show(bpawn) ++ "  White (" ++ wStrat ++ "): " ++ show(wpawn))
                             else if (pawn' /= Nothing)
                                  then if (pawn' == (Just White))
                                       then putStrLn ("White wins! Black (" ++ bStrat ++ "): " ++ show(bpawn) ++ "  White (" ++ wStrat ++ "): " ++ show(wpawn))
                                       else if (pawn' == (Just Black))
                                            then putStrLn ("Black wins! Black (" ++ bStrat ++ "): " ++ show(bpawn) ++ "  White (" ++ wStrat ++ "): " ++ show(wpawn))
                                            else putStrLn "Draw!"
                                  else putStrLn "Draw!"
                   else putStrLn "Draw!"
    else putStrLn "Draw!"

-- | The function which prints the initial GameState and initiates the game loop.
--   Takes a strategy name for each player (type String)
startGame       :: String -> String -> IO()
startGame blackChooser whiteChooser = do
    print initBoard
    gameLoop initBoard blackChooser whiteChooser

-- | Prompts the player with the valid strategy names and takes input. If input is
--   invalid, prints the valid names again and quits. Valid input starts the game.
promptForStrategy :: IO()
promptForStrategy = do
    putStr "Possible strategies:\n"
    putStr "  human\n  random\n  hostile\n  coward\n"
    putStr "Enter the strategy for BLACK:\n"
    black <- getLine
    putStr "Enter the strategy for WHITE:\n"
    white <- getLine
    if (checkLegalStrategy black white)
    then startGame black white
    else putStr "  human\n  random\n  hostile\n  coward\n"

