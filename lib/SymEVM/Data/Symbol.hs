{-# LANGUAGE GADTs #-}

module SymEVM.Data.Symbol where

newtype B256 = B256 ()

data Symbol typ where
  CB256 :: ()          -> Symbol B256
  SB256 :: String      -> Symbol B256
  Plus  :: Symbol B256 -> Symbol B256 -> Symbol B256
  Eq    :: Symbol typ  -> Symbol typ  -> Symbol Bool
