module Latex.TreeBalancing where
import Data.List (sortOn)

class Weight a where
    weight :: a -> Rational

instance Weight Char where
    weight _ = 1

class Weight a => Center a where
    center :: a -> Rational
    center _ = 0

instance Weight a => Weight [a] where
    weight = foldr ((+) . weight) 0

class Center a => Balancible a where
    permutation :: a -> [a]
    reBalance :: a -> a
    reBalance = head . sortOn (abs . center) . permutation
