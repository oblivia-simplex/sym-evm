module SymEVM.Functional.Interface.FStack where

class FStack s where
  push :: s a -> a -> s a
  pop  :: s a -> (a, s a)
