{-# LANGUAGE TypeSynonymInstances #-}

module SymEVM.Functional.Impl.Sym.FStackImpl where

import SymEVM.Data.Sym.Stack
import SymEVM.Functional.Interface.FStack

instance FStack Stack where
  push s x = x : s
  pop  s   = let (h : t) = s in (h, t)

