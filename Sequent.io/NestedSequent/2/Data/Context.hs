module Data.Context where

import Data.Either ( lefts, rights )
import Data.Either3 ( Either3(..) )
import Data.Maybe ( mapMaybe )
import Control.Monad ( guard )
import Data.List ( sort )
import Class.Normalizable ( Normalizable(normalize) )
import Control.Monad.Cont (Cont)

------------------ Atom ------------------
data Var = Var String | Bar String deriving (Eq, Ord, Show)

class Negatible a where
    negation :: a -> a

instance Negatible Var where
    negation (Var a) = Bar a
    negation (Bar a) = Var a

instance ToFormula Var where
    toFormula = Atom

instance ToNestedSequent Var where
    toNestedSequent = toNestedSequent . toFormula

instance ToContext Var where
    toContext = toContext . toNestedSequent

------------------ Formula ------------------
data Formula = Atom Var
             | And Formula Formula
             | Or Formula Formula
             | Diamond Formula
             | Box Formula
             deriving (Eq, Ord, Show)

class ToFormula a where
    toFormula :: a -> Formula

bottom = And (Atom $ Var "A") (Atom $ Bar "A")

top = Or (Atom $ Var "A") (Atom $ Bar "A")

instance Negatible Formula where
    negation (Atom a)    = Atom    (negation a)
    negation (And a b)   = Or      (negation a) (negation b)
    negation (Or a b)    = And     (negation a) (negation b)
    negation (Diamond a) = Box     (negation a)
    negation (Box a)     = Diamond (negation a)

instance ToFormula Formula where
    toFormula = id


instance ToNestedSequent Formula where
    toNestedSequent (Atom a) = NestedSequent [Left (Atom a)]
    toNestedSequent (Or a b) = NestedSequent [Left a, Left b]
    toNestedSequent (And a b) = NestedSequent [Left (And a b)]
    toNestedSequent (Diamond a) = NestedSequent [Left (Diamond a)]
    toNestedSequent (Box a) = NestedSequent [Right (toNestedSequent a)]

instance ToContext Formula where
    toContext = toContext . toNestedSequent

------------------ NestedSequent ------------------
newtype NestedSequent = NestedSequent [Either Formula NestedSequent] deriving (Eq, Ord, Show)

class ToNestedSequent a where
    toNestedSequent :: a -> NestedSequent

class Wrapable a where
    wrap :: a -> a

instance Wrapable NestedSequent where
    wrap s = NestedSequent [Right s]

instance ToFormula NestedSequent where
    toFormula (NestedSequent xs) =
        case xs of
            []       -> bottom
            (x : xs) -> foldr (Or . toTerm) (toTerm x) xs where
                toTerm (Left k)  = k
                toTerm (Right k) = Box (toFormula k)

instance ToNestedSequent NestedSequent where
    toNestedSequent = id

instance ToContext NestedSequent where
    toContext (NestedSequent xs) = Context $ fmap toTerm xs where
        toTerm (Left a) = Second a
        toTerm (Right a) = Third $ toContext a


------------------ Context ------------------
type Hole = Int
newtype Context = Context [Either3 Hole Formula Context] deriving (Eq, Ord, Show)


class ToContext a where
    toContext :: a -> Context

instance ToContext Context where
    toContext = id


contextToMaybeNestedSequent :: Context -> Maybe NestedSequent
contextToMaybeNestedSequent (Context xs) = do
    converted <- traverse convert xs
    return (NestedSequent converted)
        where
            convert :: Either3 Hole Formula Context -> Maybe (Either Formula NestedSequent)
            convert (First _)   = Nothing
            convert (Second f)  = Just (Left f)
            convert (Third ctx) = Right <$> contextToMaybeNestedSequent ctx

instance Normalizable Context where
    normalize :: Context -> Context
    normalize (Context xs) = Context $ sort $ aux 0 xs where
        aux m []     = []
        aux m (First _ : xs) = First m : aux (m + 1) xs
        aux m (Second f : xs) = Second f : aux m xs
        aux m (Third (Context ys) : xs) = Third (Context ys') : aux (m' + 1) xs where
            ys' = aux m ys
            m' = maximum $ mapMaybe toTerm ys'
            toTerm (First k) = Just k
            toTerm _         = Nothing

fill :: Context -> Hole -> [Context] -> Context
fill (Context xs) h cs = Context $ aux xs h cs where
    aux (First g : xs) h cs
        | h == g = fmap Third cs ++ aux xs h cs
        | otherwise = First g : aux xs h cs
    aux (Third ys : xs) h cs = Third (fill ys h cs) : aux xs h cs
    aux (x : xs) h cs = x : aux xs h cs
    aux [] _ _ = []

instance Wrapable Context where
    wrap t = Context [Third t]


depth :: (Num a, Ord a) => Context -> a
depth (Context ls) = minimum (map val ls) where
    val :: (Num a, Ord a) => Either3 Hole Formula Context -> a
    val (First _) = 0
    val (Second _) = 0
    val (Third (Context ctx)) = 1 + minimum (map val ctx)