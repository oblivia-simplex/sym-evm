module SymEVM.Data.Util.Symbol where

data Symbol
  = CB256 ()
  | SB256 String
  | Plus Symbol Symbol
  | Eq Symbol Symbol deriving ( Eq, Ord )



{- How do I get Eq instance for GADTs? Until I can figure this out, i'll use normal ADTs... 


{-# LANGUAGE GADTs #-}


module SymEVM.Data.Sym.Symbol where

newtype B256 = B256 () deriving ( Eq )

data Symbol typ where
  CB256 :: ()          -> Symbol B256
  SB256 :: String      -> Symbol B256
  Plus  :: Symbol B256 -> Symbol B256 -> Symbol B256
  Eq    :: Symbol typ  -> Symbol typ  -> Symbol Bool


instance Eq (Symbol typ) where
  (CB256 x) == (CB256 y) = x == y
  (SB256 x) == (SB256 y) = x == y
  (Plus a b) == (Plus c d) = (a == c) && (b == d)
  (Eq (CB256 a) (CB256 b))   == (Eq (CB256 c) (CB256 d)) = (a == c) && (b == d)
  (Eq (CB256 a) (SB256 b))   == (Eq (CB256 c) (SB256 d)) = (a == c) && (b == d)
  _ == _ = False
  -- (Eq (CB256 a) (SB256 b))   == (Eq c d) = True
  --(a == c) && (b == d)

-}
