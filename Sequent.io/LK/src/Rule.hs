module Rule where

import Proposition
import Sequent
import ProofTree
import Data.List (intersect)
import Prelude

can :: Maybe a -> a
can (Just a) = a

haveCommonElements :: Eq a => [a] -> [a] -> Bool
haveCommonElements xs ys = not (null (xs `intersect` ys))

newVar :: (Enum a, Eq a) => [a] -> a
newVar var = aux var (toEnum 65) where
    aux var n
        | n `elem` var = aux var (succ n)
        | otherwise    = n

type Rule a = ProofTree a -> Maybe (ProofTree a)

rules :: (Eq a, Enum a) => [Rule a]
rules = [identity, notL, notR, andL, andR, orL, orR, implyL, implyR, forallL, forallR, existsL, existsR, exchangeL, exchangeR, shuffleVar]

identity :: Eq a => Rule a
identity (Open s@(Sequent var gamma delta))
    | haveCommonElements gamma delta = Just $ Rule1 s Close "identity"
    | otherwise = Nothing
identity _ = Nothing

exchangeL :: Rule a
exchangeL (Open s@(Sequent var (p : q : gamma) delta)) =
    Just $ Rule1 s (Open $ Sequent var (q : p : gamma) delta) "exchangeL"
exchangeL _ = Nothing

exchangeR :: Rule a
exchangeR (Open s@(Sequent var gamma (p : q : delta))) =
    Just $ Rule1 s (Open $ Sequent var gamma (q : p : delta)) "exchangeR"
exchangeR _ = Nothing

contractionL :: Rule a
contractionL (Open s@(Sequent var (p : gamma) delta)) =
    Just $ Rule1 s (Open $ Sequent var (p : p : gamma) delta) "contractionL"
contractionL _ = Nothing

contractionR :: Rule a
contractionR (Open s@(Sequent var gamma (p : delta))) =
    Just $ Rule1 s (Open $ Sequent var gamma (p : p : delta)) "contractionR"
contractionR _ = Nothing

notL :: Rule a
notL (Open s@(Sequent var (Not p : gamma) delta)) =
    Just $ Rule1 s (Open $ Sequent var gamma (p : delta)) "notL"
notL _ = Nothing

notR :: Rule a
notR (Open s@(Sequent var gamma (Not p : delta))) =
    Just $ Rule1 s (Open $ Sequent var (p : gamma) delta) "notR"
notR _ = Nothing

andL :: Rule a
andL (Open s@(Sequent var (And p q : gamma) delta)) =
    Just $ Rule1 s (Open $ Sequent var (p : q : gamma) delta) "andL"
andL _ = Nothing

andR :: Rule a
andR (Open s@(Sequent var gamma (And p q : delta))) =
    Just $ Rule2 s (Open $ Sequent var gamma (p : delta)) (Open $ Sequent var gamma (q : delta)) "andR"
andR _ = Nothing

orL :: Rule a
orL (Open s@(Sequent var (Or p q : gamma) delta)) =
    Just $ Rule2 s (Open $ Sequent var (p : gamma) delta) (Open $ Sequent var (q : gamma) delta) "orL"
orL _ = Nothing

orR :: Rule a
orR (Open s@(Sequent var gamma (Or p q : delta))) =
    Just $ Rule1 s (Open $ Sequent var gamma (p : q : delta)) "orR"
orR _ = Nothing

implyL :: Rule a
implyL (Open s@(Sequent var (Imply p q : gamma) delta)) =
        Just $ Rule2 s (Open $ Sequent var gamma (p : delta)) (Open $ Sequent var (q : gamma) delta) "implyL"
implyL _ = Nothing

implyR :: Rule a
implyR (Open s@(Sequent var gamma (Imply p q : delta))) =
    Just $ Rule1 s (Open $ Sequent var (p: gamma) (q : delta)) "implyR"
implyR _ = Nothing

shuffleVar :: Rule a
shuffleVar (Open s@(Sequent (var : vars) gamma delta)) =
    Just $ Rule1 s (Open $ Sequent (vars ++ [var]) gamma delta) "sh"
shuffleVar _ = Nothing

forallL :: Eq a => Rule a
forallL (Open s@(Sequent v@(var : vars) (Forall x p : gamma) delta)) =
    Just $ Rule1 s (Open $ Sequent v (substitute x var p : gamma) delta) "forallL"
forallL _ = Nothing

forallR :: (Enum a, Eq a) => Rule a
forallR (Open s@(Sequent v gamma (Forall x p : delta))) =
    Just $ Rule1 s (Open $ Sequent (newVariable : v) gamma (substitute x newVariable p : delta)) "forallR" where
        newVariable = newVar v
forallR _ = Nothing

existsL :: (Enum a, Eq a) => Rule a
existsL (Open s@(Sequent v gamma (Exists x p : delta))) =
    Just $ Rule1 s (Open $ Sequent (newVariable : v) (substitute x newVariable p : gamma) delta) "forallR" where
        newVariable = newVar v
existsL _ = Nothing

existsR :: Eq a => Rule a
existsR (Open s@(Sequent v@(var : vars) gamma (Exists x p : delta))) =
    Just $ Rule1 s (Open $ Sequent v gamma (substitute x var p : delta)) "existsR"
existsR _ = Nothing
