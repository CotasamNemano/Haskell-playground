import Data.Char (chr, ord)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Data.List (nub)


vietaGrouping :: Int -> [a] -> [[a]]
vietaGrouping n ls@(x:xs)
    |n == 0    = [[]]
    |n >  len  = [[]]
    |n == len  = [ls]
    |otherwise = map (x:) (vietaGrouping (n - 1) xs) ++ vietaGrouping n xs
    where len = length ls


vietaSum :: Num a => Int -> [a] -> a
vietaSum n ls = sum $ map product $ vietaGrouping n ls


vietaChracterics :: Num a => [a] -> [a]
vietaChracterics ls = map (\n -> vietaSum n ls) [1..length ls]


mapIntToChr :: (Int -> Int) -> Char -> Char
mapIntToChr f x = chr $ f $ ord x


textZone :: Int -> Int
textZone n = zoneBreak 91 97 $ zoneBreak 58 65 (mod n 60 + 48)


zoneBreak :: Int -> Int -> Int -> Int
zoneBreak pos run n
    | n >= pos = n + (run - pos)
    | otherwise = n


trim :: Int -> [Int] -> [Int]
trim n ls = aux operators (take n ls)
    where
        operators :: [Int -> Int]
        operators = map (\x -> \y -> (x^2 + y)) $ drop n ls
        aux :: [Int -> Int] -> [Int] -> [Int]
        aux [] ls = ls
        aux (f : fs) ls = aux fs (map f ls)


enlength :: Int -> [Char] -> [Char]
enlength n ls = ls ++ baseMask n ++ ls


vietaHash :: Int -> [Char] -> [Char]
vietaHash n = map chr . map textZone . vietaChracterics . trim n . map ord . enlength n


eyeEase :: Int -> [Char] -> [Char]
eyeEase n ls = aux n ls
    where
        aux _ [] = []
        aux 0 (x : xs) = '-' : x : aux (n - 1) xs
        aux k ls@(x : xs) =
            if length ls <= k
               then ls
               else x : aux (k - 1) xs


baseMask :: Int -> [Char]
baseMask n = map chr $ map textZone [0..n]


passGen :: Int -> Int -> [Char] -> [Char]
passGen m n = eyeEase m . vietaHash n


qVietaHash :: Int -> Int -> [Char] -> [Char]
qVietaHash m n ils = aux n (enlength (m + n - 4) ils)
    where
        aux l ls
            |length ls == n - 1  = []
            |otherwise       =
                let (x : xs) = headVietaHash n ls
                in x : aux l xs


headVietaHash :: Int -> [Char] -> [Char]
headVietaHash n ls = vietaHash n (take n ls) ++ drop n ls


main :: IO ()
main = do
    args <- getArgs
    putStrLn $ passGen 4 (read $ args !! 0) (args !! 1)
    exitSuccess
