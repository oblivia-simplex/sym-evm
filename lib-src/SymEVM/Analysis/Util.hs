module SymEVM.Analysis.Util where

import qualified Data.Set as S
import qualified Data.Either as E

partitionEithers :: (Ord a, Ord b) => S.Set (Either a b) -> (S.Set a, S.Set b)
partitionEithers s = 
  let sL     = S.toList s
      (l, r) = E.partitionEithers sL in
      (S.fromList l, S.fromList r)

  
