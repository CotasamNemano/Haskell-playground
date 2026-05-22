module Data.Either3 where

data Either3 a b c where
  First  :: a -> Either3 a b c
  Second :: b -> Either3 a b c
  Third  :: c -> Either3 a b c
  deriving (Show, Eq, Ord)
