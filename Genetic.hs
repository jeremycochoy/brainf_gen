module Genetic
  ( module Genetic
  ) where

import           Types
import           System.Random
import           Control.Lens
import           Data.Char

-- A fitness function count how much a program was 'efficient'
-- according to some contrainsts (output, number of instructions, etc…)
type FitnessFunction = StateMachine -> Int

{--------------------------------------------}
{- Some simple fitness functions generators -}
{--------------------------------------------}

textFitness :: String -> FitnessFunction
textFitness str vm = diffLen + diffChar + empty
  where
    out :: String
    out = vm ^. output
    outLen = length out
    aimLen = length str
    charCmp (a, b) = abs (ord a - ord b)
    diffLen = 250 * abs (aimLen - outLen)
    diffChar = sum . map charCmp $ zip str out
    empty = if out == "" then 1000 else 0

{---------------------------------------------------}
{- Part related to generatic random peaces of code -}
{---------------------------------------------------}


--Probabilities for each symbol to occure
-- (All the value are summed and the probability is
--  value_symbol / sum of values).
pbList :: [(Sym, Double)]
pbList = [ (SPlus, 100)
         , (SMinus, 100)
         , (SLeft, 50)
         , (SRight, 50)
         , (SDot, 10)
         , (SComma, 1)
         , (SScope, 5)
         , (SUnscope, 5)
         ]
-- Function that sum all values
pbSumFct :: [(Sym, Double)] -> Double
pbSumFct = sum . map snd
-- The sum of all values
pbSum :: Double
pbSum = pbSumFct pbList
--Sum nb first values and divide by the sum of all values
pbCompute :: Int -> [(Sym, Double)] -> Double
pbCompute nb li = pbSumFct (take nb li) / pbSum

--Select the sym opcode from a random value r in [0, 1]
selectSym :: Double -> Sym
selectSym r = selectSymAux r pbList 0
  where
    selectSymAux r [(x, _)] _ = x
    selectSymAux r (x : xs) sum = if r < sum' / pbSum then
                                    sym
                                  else
                                    selectSymAux r xs sum'
      where
        sum' = sum + val
        (sym, val) = x


instance Random Sym where
  random g = case random g of
    (r, g') -> (selectSym r, g')
  randomR = undefined


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
