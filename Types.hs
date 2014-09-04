{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Types
    ( module Types
    ) where

import           Control.Lens
import           Control.Monad.State
import           Data.Default as Types
import           Data.Int

data Sym = SPlus
         | SMinus
         | SLeft
         | SRight
         | SDot
         | SComma
         | SScope
         | SUnscope
         | SUndef
         deriving (Eq)
instance Show Sym where
  show sym = case sym of
    SPlus    -> "+"
    SMinus   -> "-"
    SLeft    -> "<"
    SRight   -> ">"
    SDot     -> "."
    SComma   -> ","
    SScope   -> "["
    SUnscope -> "]"
    SUndef   -> " "

data Code = Code [Sym] deriving (Eq)

instance Show Code where
  show (Code code) = concat . map show $ code

instance Read Code where
  readsPrec _ str = [(Code $ readAux str, "")]
    where readAux [] = []
          readAux (c : cs) = select c : readAux cs
          select '+' = SPlus
          select '-' = SMinus
          select '<' = SLeft
          select '>' = SRight
          select '.' = SDot
          select ',' = SComma
          select '[' = SScope
          select ']' = SUnscope
          select _   = SUndef

data Strip = Strip
  { s_current  :: !Int8
  , s_next     :: [Int8]
  , s_prev     :: [Int8]
  } deriving (Show, Eq)

inc strip = strip { s_current = (s_current strip) + 1}
dec strip = strip { s_current = (s_current strip) - 1}
next (Strip c (nh : nt) prev) = Strip nh nt (c : prev)
prev (Strip c next (ph : pt)) = Strip ph (c : next) pt

--DIsplay localy the strip (n, value, n)
showStrip n strip = show first ++ " " ++ show cur ++ " " ++ show second
  where
    first  = reverse . take n . s_prev $ strip
    second = take n . s_next $ strip
    cur    = s_current strip

instance Default Strip where
  def = Strip 0 (repeat 0) (repeat 0)

data StateMachine = StateMachine
  { _code      :: !Code
  , _code_tail :: !Code
  , _nb_insts  :: !Int
  , _strip     :: !Strip
  , _output    :: !String
  } deriving (Show, Eq)

instance Default StateMachine where
  def = StateMachine (Code []) (Code []) 0 def ""

type StMaS = State StateMachine

makeClassy ''StateMachine


