{-# LANGUAGE LambdaCase #-}

module Runner
    ( module Runner
    ) where

import           Control.Lens
import           Types
import           Data.Char

exec :: Int -> StMaS ()
exec lim = do
  -- Read the next instruction, and run it
  sym <- readInst
  case sym of
    SPlus    -> do
      strip %= inc
    SMinus   -> do
      strip %= dec
    SLeft    -> do
      strip %= prev
    SRight   -> do
      strip %= next
    SDot     -> do
      s <- use strip
      output <>= [chr . fromIntegral $ s_current s]
    SComma   -> undefined
    SScope   -> do
      s <- use strip
      case s_current s of
        0 -> gotoEndScope
        _ -> return ()
    SUnscope -> do
      s <- use strip
      case s_current s of
        0 -> return ()
        _ -> gotoStartScope
    _        -> return ()
  -- Count the instruction
  insts <- nb_insts <+= 1
  -- Choose to continue executing or stop when code finished
  case sym of
    SUndef -> return ()
    _ | insts >= lim -> return ()
    _ | otherwise    -> exec lim

-- Go back to the next instruction after the previous [ related
gotoStartScope :: StMaS ()
gotoStartScope = unreadInst >> gotoStartScopeAux 0

-- Go back to the next closing ]
gotoEndScope :: StMaS ()
gotoEndScope = gotoEndScopeAux 0

-- Implementation : Count the [ and ].
gotoEndScopeAux :: Int -> StMaS ()
gotoEndScopeAux n = do
      sym <- readInst
      -- If it's a [ or ], we increase / decrease the counter
      case sym of
        SScope   -> gotoEndScopeAux (n + 1)
        SUnscope -> case n of
                      -- When the counter reach 0, it means that
                      -- we are at the right ].
                      0 -> return ()
                      _ -> gotoEndScopeAux (n - 1)
        _        -> gotoEndScopeAux n

-- Implementation : Count the [ and ].
gotoStartScopeAux :: Int -> StMaS ()
gotoStartScopeAux n = do
      sym <- unreadInst
      case sym of
        SUnscope   -> gotoStartScopeAux (n + 1)
        SScope -> case n of
                      0 -> return ()
                      _ -> gotoStartScopeAux (n - 1)
        _        -> gotoStartScopeAux n

readInst :: StMaS Sym
readInst = do
  use code >>= \case
    Code (i : t) -> do
      code .= Code t
      pushCodeTail i
      return i
    Code []      -> return SUndef
  where
    pushCodeTail i = code_tail %= \(Code t) -> Code (i:t)

unreadInst :: StMaS Sym
unreadInst = do
  use code_tail >>= \case
    Code (i : t) -> do
      code_tail .= Code t
      pushCode i
      return i
    Code []      -> return SUndef
  where
    pushCode i = code %= \(Code t) -> Code (i:t)
