module Balancing where

class Weight a where
    weight :: a -> Rational

instance Weight Char where
    weight _ = 1

class Weight a => Center a where
    center :: a -> Rational
    center _ = 0

instance Weight a => Weight [a] where
    weight = foldr (+) 0 . map weight
