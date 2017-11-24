{- |
Module      : ApocStrategyRandom
Description : Module containing an AI strategy for Apocalypse which moves randomly
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

module ApocStrategyRandom (
   random
   ) where

import ApocTools
import ApocFunctions

-- | Returns a random move choice based on the given GameState, Playtype and Player
random    :: Chooser
random g Normal p = return (randomMove g (0,0) (0,0) p 0 0)
random g PawnPlacement p = return (randomPlace g (2,2) p 0 0)

