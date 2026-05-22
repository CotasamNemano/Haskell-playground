{-# LANGUAGE KindSignatures #-}

import Data.Kind (Type)
import Data.List (sort)
import Data.List.Ordered (minus)
type Occur = Int

primesEU = 2 : eulers [3,5..]
    where
        eulers (p:xs) = p : eulers (xs `minus` map (p*) (p:xs))

data MultiSet a = MultiSet Int [(MultiSet a, a)] deriving (Ord)

instance (Eq a, Ord a, Num a) => Eq (MultiSet a) where
    (==) (MultiSet m []) (MultiSet n []) = m == n
    (==) x y = (==) a b where
        MultiSet a = normalize x
        MultiSet b = normalize y

empty = MultiSet 0 []

toOccurList :: (Ord a, Num a) => MultiSet a -> [(MultiSet a, a)]
toOccurList (MultiSet m x) = aux empty 0 (sort x) where
    aux term acc []       = [(term, acc)]
    aux term acc (x : xs) | term == fst x = aux term (acc + snd x) xs
                          | otherwise     = (term, acc) : aux (normalize $ fst x) (snd x) xs

normalize :: (Ord a, Num a) => MultiSet a -> MultiSet a
normalize s@(MultiSet m _)= MultiSet m $ toOccurList s

unaryOp :: (Eq b, Integral b) => b ->  MultiSet a ->  MultiSet a
unaryOp 0 _            = empty
unaryOp n (MultiSet m x) = MultiSet [(unaryOp (n - 1) t, o) | (t, o) <- x]

binaryOp :: (Eq b, Integral b, Num a) => b -> MultiSet a ->  MultiSet a ->  MultiSet a
binaryOp 0 (MultiSet a) (MultiSet b) = MultiSet (a ++ b)
binaryOp n (MultiSet a) (MultiSet b) = MultiSet
    [(binaryOp (n - 1) t1 t2, o1 * o2) | (t1, o1) <- a, (t2, o2) <- b]

(^*) = binaryOp 2
infixl 8 ^*

instance (Ord a, Num a, Show a) => Show (MultiSet a) where
    show x | count /= 0 && tailString /= [] = show count ++ " + " ++ tailString
           | count == 0 && tailString /= [] = tailString
           | otherwise                      = show count
        where
            MultiSet y = normalize x
            (_, count) = head y
            tailString = concatTail $ map (\(t, o) ->
                ((if o /= 1 then show o else "") ++ "a^(" ++ show t ++ ")")) (tail y)

            concatTail [] = ""
            concatTail [x] = x
            concatTail (x : xs) = x ++ " + " ++ concatTail xs


instance Num a => Num (MultiSet a) where
    (+) = binaryOp 0
    (*) = binaryOp 1
    negate (MultiSet ls) = MultiSet (fmap (\(t, o) -> (t, negate o)) ls)
    abs    (MultiSet ls) = MultiSet (fmap (\(t, o) -> (t, abs    o)) ls)
    signum (MultiSet ls) = MultiSet (fmap (\(t, o) -> (t, signum o)) ls)
    fromInteger 0 = empty
    fromInteger n = MultiSet [(empty, fromInteger n)]

a = MultiSet [(MultiSet [(empty, 1)],1)]
up x = MultiSet [(x, 1)]

jump 0 x = x
jump n x = jump (n - 1) (MultiSet [(x, 1)])

inside :: (Ord a, Num a) => ([(MultiSet a, a)] -> [(MultiSet a, a)]) -> MultiSet a -> MultiSet a
inside f x = MultiSet (f y) where
    y = toOccurList x

k i = take i $ fmap (inside (take i) . sum . take i . fmap (a^)) [fmap f [0 .. ] | f <- fmap (^) $ take i primesEU]
t i = inside (take 50) $ foldr (binaryOp 2) a (k i)
