module SymEVM.Analysis.Util where

import qualified Data.Set as S
import qualified Data.Either as E
import qualified Data.ByteString.Lazy as B

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

pad256 :: B.ByteString -> B.ByteString
pad256 b =
  let len = B.length b in
  if len < 32 then
    let diff  = 32 - len
        zeros = B.replicate diff 0x00 
    in
    
    B.append zeros b

    
  else
    b
