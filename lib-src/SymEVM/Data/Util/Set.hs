module SymEVM.Data.Util.Set (Set, empty, null, singleton, insert, union, map, foldl, partition) where

import Prelude hiding (map, foldl, null)
import qualified Data.List as D

newtype Set a = Set [a]

instance Eq a => Eq (Set a) where
  xs == ys =
    let Set xs' = xs in
    let Set ys' = ys in
    xs' == ys'

instance Ord a => Ord (Set a) where
  compare xs ys =
    let Set xs' = xs in
    let Set ys' = ys in
    compare xs' ys'

instance Show a => Show (Set a) where
  show xs =
    let Set xs' = xs in
    show xs'

empty :: Set a
empty = Set []

null :: Set a -> Bool
null xs =
  let Set xs' = xs in
  D.null xs'

singleton :: a -> Set a
singleton x = Set [x]

insert :: Ord a => a -> Set a -> Set a
insert x xs =
  let Set xs' = xs in
  if x `elem` xs' then
    xs
  else
    Set (x : xs')

union :: Ord a => Set a -> Set a -> Set a
union s1 s2 = 
  let Set s1' = s1 in
  let Set s2' = s2 in
  Set (D.nub (s1' ++ s2'))

map :: Ord b => (a -> b) -> Set a -> Set b
map f xs =
  let Set xs' = xs in
  Set (D.map f xs')

foldl :: (a -> b -> a) -> a -> Set b -> a
foldl f init xs =
  let Set xs' = xs in
  D.foldl f init xs'

partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition f xs =
  let Set xs' = xs in
  let (ts, fs) = D.partition f xs' in
  (Set ts, Set fs)


