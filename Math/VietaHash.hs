import Data.Char (chr, ord)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Data.List (nub)


loopIndex :: [a] -> Int -> a
loopIndex ls = (!!) ls . rev mod (length ls)

rev :: (a -> b -> c) -> b -> a -> c
rev f x y = f y x

loopTake :: Int -> [a] -> [a]
loopTake 0 ls = []
loopTake n ls = loopIndex ls (length ls - n) : loopTake (n - 1) ls


eyeEase :: Int -> [Char] -> [Char]
eyeEase n ls = aux n ls
    where
        aux _ [] = []
        aux 0 (x : xs) = '-' : x : aux (n - 1) xs
        aux k ls@(x : xs) =
            if length ls <= k
               then ls
               else x : aux (k - 1) xs

zoneBreak :: Int -> Int -> Int -> Int
zoneBreak pos run n
    | n >= pos = n + (run - pos)
    | otherwise = n

textZone :: Int -> Int
textZone n = zoneBreak 91 97 $ zoneBreak 58 65 (mod n 60 + 48)

type Reduction a = (a, a, a, a) -> a
type BiReduction a = (Reduction a, Reduction a)

sReduction :: Num a => Reduction a
sReduction (x, y, z, w) = x*y + x*z + x*w + y*z + y*w + z*w

rReduction :: Num a => Reduction a
rReduction (x, y, z, w) = x*y*z + x*y*w + y*z*w

hhReduce :: Num a => BiReduction a -> [a] -> [a]
hhReduce (f, g) (x : y : z : w : ws) =
    let (i, j) = (f (x, y, z, w), g (x, y, z, w))
    in i : j : ws

hashInt :: BiReduction Int -> [Int] -> [Int]
hashInt f ls@(x : y : z : w : ws) = t : hashInt f ts
    where (t : ts) = hhReduce f ls
hashInt f ls = ls


hash :: BiReduction Int -> Int -> [Char] -> [Char]
hash f n = map chr . map textZone . hashInt f . hashInt f . zipWith (+) [0..16*2^n - 1] . map ord . loopTake (16*2^n)

passGen :: Int -> Int -> [Char] -> [Char]
passGen m n = eyeEase m . hash (sReduction, rReduction) n


main :: IO ()
main = do
    args <- getArgs
    putStrLn $ passGen (read $ args !! 0) (read $ args !! 1) (args !! 2)
    exitSuccess

