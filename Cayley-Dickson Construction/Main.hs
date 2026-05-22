module Main where

newtype CDC a = CDC (a, a) deriving (Eq)

class Num a => ComplexLike a where
    conjugate :: a -> a
    e :: Int -> a
    component :: Fractional k => Int -> a -> k
    dimension :: a -> Int
    norm :: (Floating k, Fractional k) => a -> k

instance ComplexLike Rational where
    conjugate = id
    e _ = 1
    dimension _ = 1
    component _ = fromRational
    norm = fromRational . abs

instance ComplexLike a => Num (CDC a) where
    (+) (CDC (x, y)) (CDC (z, w)) = CDC (x + z, y + w)
    (-) (CDC (x, y)) (CDC (z, w)) = CDC (x - z, y - w)
    (*) (CDC (x, y)) (CDC (z, w)) = CDC (x * z - conjugate w * y, w * x + y * conjugate z)
    signum (CDC (x, y)) = CDC (signum x, signum y)
    abs (CDC (x, y)) = CDC (abs x, abs y)
    fromInteger n = CDC (fromInteger n, 0)

instance ComplexLike a => ComplexLike (CDC a) where
    conjugate (CDC (x, y)) = CDC (conjugate x, -y)
    e i = case quotRem i 2 of
        (n, 0) -> CDC (e n, 0)
        (n, 1) -> CDC (0, e n)
    dimension (CDC (x, _)) = 2 * dimension x
    component i (CDC (x, y)) = case quotRem i 2 of
        (n, 0) -> component n x
        (n, 1) -> component n y
    norm (CDC (x, y)) = sqrt (norm x ^ 2 + norm y ^ 2)

instance (Show a, ComplexLike a) => Show (CDC a) where
    show x = comsump $ map (`component` x) [0 .. dimension x - 1] where
        comsump = aux 0 where
            aux 0 [n] = show n
            aux 0 (n : ns) = show n ++ " + " ++ aux 1 ns
            aux k [n] = show n ++ "e" ++ show k
            aux k (n : ns) = show n ++ "e" ++ show k ++ " + " ++ aux (k + 1) ns



    


main :: IO ()
main = return ()