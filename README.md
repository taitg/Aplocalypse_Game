CPSC 449 Project 1 - Haskell Apocalypse
Date:           Feb 21 2016
Tutorial:       Tutorial 01
Team:           #44
Description:    An Apocalypse game made for CPSC 449, templates provided by Robert Kremer at U of C
Contributors:
                Robert Ranger - 10137850
                Travis Paquette
                Geordie Tait
                Rumen Kasabov

Compiling:      ghc Apoc.hs -o Apoc

Usage:          To start the game in interactive mode do 
                ./Apoc
                To start a game with two given strategies do
                ./Apoc {strategy} {strategy}

Strategies:     random
                human
                hostile
                coward

FileList:       Apoc.hs                     
                ApocFunctions.hs 
                ApocStrategyCoward.hs 
                ApocStrategyHostile.hs 
                ApocStrategyHuman.hs 
                ApocStrategyRandom.hs  
                ApocTools.hs  
                CmdLineArgs.hs

NOTE:           Requires the System.Random module installed using cabal
                cabal update
                cabal install random
                Supports Haskell 7.10.2, or 7.10.3
