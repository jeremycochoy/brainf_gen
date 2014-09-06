module Genetic
  ( module Genetic
  ) where

import           Types
import           System.Random

-- A fitness function count how much a program was 'efficient'
-- according to some contrainsts (output, number of instructions, etc…)
type FitnessFunction = StMaS -> Int

--Probabilities for each symbol to occure
-- (All the value are summed and the probability is
--  value_symbol / sum of values).
pbPlus    = 100
pbMinus   = 100
pbLeft    = 50
pbRight   = 50
pbDot     = 10
pbComma   = 0
pbScope   = 5
pbUnscope = 5


-- Genetic simplification : +/- and >/< are merged.
simplify :: Code -> Code
simplify Code (a : b : xs) = case (a, b) of
  (SPlus, SMinus) -> r
  (SMinus, SPlus) -> r
  (SLeft, SRight) -> r
  (SRight, SLeft) -> r
  _               -> Code (a : b : r)
  where
    Code r = simplify Code xs
simplify code = code
