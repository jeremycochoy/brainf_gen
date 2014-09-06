module Genetic
  ( module Genetic
  ) where

import           Types
import           System.Random

-- A fitness function count how much a program was 'efficient'
-- according to some contrainsts (output, number of instructions, etc…)
type FitnessFunction = StMaS () -> Int

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
simplify :: [Sym] -> [Sym]
simplify (a : b : xs) = case (a, b) of
  (SPlus, SMinus) -> r
  (SMinus, SPlus) -> r
  (SLeft, SRight) -> r
  (SRight, SLeft) -> r
  _               -> a : simplify (b : xs)
  where
    r = simplify xs
simplify xs = xs

simplifyCode :: Code -> Code
simplifyCode = Code . simplify . getSyms
