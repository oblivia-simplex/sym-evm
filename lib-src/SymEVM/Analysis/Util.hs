module SymEVM.Analysis.Util where

import qualified Data.Set as S
import qualified Data.Either as E

partitionEithers :: (Ord a, Ord b) => S.Set (Either a b) -> (S.Set a, S.Set b)
partitionEithers s = 
  let sL     = S.toList s
      (l, r) = E.partitionEithers sL in
      (S.fromList l, S.fromList r)

flattenPairs :: (Ord a, Ord b) => S.Set (S.Set a, S.Set b) -> (S.Set a, S.Set b)
flattenPairs = 
  S.foldl flattenPair (S.empty, S.empty)
  where
    flattenPair acc curr =
      let (accL, accR)   = acc
          (currL, currR) = curr
      in
      (S.union accL currL, S.union accR currR)

