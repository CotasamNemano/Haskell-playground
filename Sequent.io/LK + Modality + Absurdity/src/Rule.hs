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


identity :: Eq a => Rule a
identity (Open s@(Sequent var gamma delta))
    | haveCommonElements gamma delta = Just $ Rule1 "identity" s Close
    | otherwise = Nothing
identity _ = Nothing

exchangeL :: Rule a
exchangeL (Open s@(Sequent var (p : q : gamma) delta)) =
    Just $ Rule1 "exchangeL" s (Open $ Sequent var (q : p : gamma) delta)
exchangeL _ = Nothing

exchangeR :: Rule a
exchangeR (Open s@(Sequent var gamma (p : q : delta))) =
    Just $ Rule1 "exchangeR" s (Open $ Sequent var gamma (q : p : delta))
exchangeR _ = Nothing

shuffleL :: Rule a
shuffleL (Open s@(Sequent var (p : q : gamma) delta)) =
    Just $ Rule1 "shuffleL" s (Open $ Sequent var (q : gamma ++ [p]) delta)
shuffleL _ = Nothing

shuffleR :: Rule a
shuffleR (Open s@(Sequent var gamma (p : q : delta))) =
    Just $ Rule1 "shuffleR" s (Open $ Sequent var gamma (q : delta ++ [p]))
shuffleR _ = Nothing

contractionL :: Rule a
contractionL (Open s@(Sequent var (p : gamma) delta)) =
    Just $ Rule1 "contractionL" s (Open $ Sequent var (p : p : gamma) delta)
contractionL _ = Nothing

contractionR :: Rule a
contractionR (Open s@(Sequent var gamma (p : delta))) =
    Just $ Rule1 "contractionR" s (Open $ Sequent var gamma (p : p : delta))
contractionR _ = Nothing

notL :: Rule a
notL (Open s@(Sequent var (Not p : gamma) delta)) =
    Just $ Rule1 "notL" s (Open $ Sequent var (Imply p Bottom : gamma) delta)
notL _ = Nothing

notR :: Rule a
notR (Open s@(Sequent var gamma (Not p : delta))) =
    Just $ Rule1 "notR" s (Open $ Sequent var gamma (Imply p Bottom : delta))
notR _ = Nothing

andL :: Rule a
andL (Open s@(Sequent var (And p q : gamma) delta)) =
    Just $ Rule1 "andL" s (Open $ Sequent var (p : q : gamma) delta)
andL _ = Nothing

andR :: Rule a
andR (Open s@(Sequent var gamma (And p q : delta))) =
    Just $ Rule2 "andR" s (Open $ Sequent var gamma (p : delta)) (Open $ Sequent var gamma (q : delta))
andR _ = Nothing

orL :: Rule a
orL (Open s@(Sequent var (Or p q : gamma) delta)) =
    Just $ Rule2 "orL" s (Open $ Sequent var (p : gamma) delta) (Open $ Sequent var (q : gamma) delta)
orL _ = Nothing

orR :: Rule a
orR (Open s@(Sequent var gamma (Or p q : delta))) =
    Just $ Rule1 "orR" s (Open $ Sequent var gamma (p : q : delta))
orR _ = Nothing

implyL :: Rule a
implyL (Open s@(Sequent var (Imply p q : gamma) delta)) =
        Just $ Rule2 "implyL" s (Open $ Sequent var gamma (p : delta)) (Open $ Sequent var (q : gamma) delta)
implyL _ = Nothing

implyR :: Rule a
implyR (Open s@(Sequent var gamma (Imply p q : delta))) =
    Just $ Rule1 "implyR" s (Open $ Sequent var (p : gamma) (q : delta))
implyR _ = Nothing

shuffleVar :: Rule a
shuffleVar (Open s@(Sequent (var : vars) gamma delta)) = Just $ Open $ Sequent (vars ++ [var]) gamma delta
shuffleVar _ = Nothing

forallL :: Eq a => Rule a
forallL (Open s@(Sequent v@(var : vars) (Forall x p : gamma) delta)) =
    Just $ Rule1 "forallL" s (Open $ Sequent v (substitute x var p : gamma) delta)
forallL _ = Nothing

forallR :: (Enum a, Eq a) => Rule a
forallR (Open s@(Sequent v gamma (Forall x p : delta))) =
    Just $ Rule1 "forallR" s (Open $ Sequent (newVariable : v) gamma (substitute x newVariable p : delta)) where
        newVariable = newVar v
forallR _ = Nothing

existsL :: (Enum a, Eq a) => Rule a
existsL (Open s@(Sequent v (Exists x p : gamma) delta)) =
    Just $ Rule1 "existsL" s (Open $ Sequent (newVariable : v) (substitute x newVariable p : gamma) delta) where
        newVariable = newVar v
existsL _ = Nothing

existsR :: Eq a => Rule a
existsR (Open s@(Sequent v@(var : vars) gamma (Exists x p : delta))) =
    Just $ Rule1 "existsR" s (Open $ Sequent v gamma (substitute x var p : delta))
existsR _ = Nothing

mNotR :: Rule a
mNotR (Open s@(Sequent v gamma (World x (MNot p) : delta))) =
    Just $ Rule1 "mNotR" s (Open $ Sequent v gamma (Not (World x p) : delta))
mNotR _ = Nothing

mNotL :: Rule a
mNotL (Open s@(Sequent v (World x (MNot p) : gamma) delta)) =
    Just $ Rule1 "mNotL" s (Open $ Sequent v (Not (World x p) : gamma) delta)
mNotL _ = Nothing

mAndR :: Rule a
mAndR (Open s@(Sequent v gamma (World x (MAnd p q) : delta))) =
    Just $ Rule1 "mAndR" s (Open $ Sequent v gamma (And (World x p) (World x q) : delta))
mAndR _ = Nothing

mAndL :: Rule a
mAndL (Open s@(Sequent v (World x (MAnd p q) : gamma) delta)) =
    Just $ Rule1 "mAndL" s (Open $ Sequent v gamma (And (World x p) (World x q) : delta))
mAndL _ = Nothing

mOrR :: Rule a
mOrR (Open s@(Sequent v gamma (World x (MOr p q) : delta))) =
    Just $ Rule1 "mOrR" s (Open $ Sequent v gamma (Or (World x p) (World x q) : delta))
mOrR _ = Nothing

mOrL :: Eq a => Rule a
mOrL (Open s@(Sequent v (World x (MOr p q) : gamma) delta)) =
    Just $ Rule1 "mOrL" s (Open $ Sequent v (Or (World x p) (World x q) : gamma) delta)
mOrL _ = Nothing

mImplyR :: Rule a
mImplyR (Open s@(Sequent v gamma (World x (MImply p q) : delta))) =
    Just $ Rule1 "mImplyR" s (Open $ Sequent v gamma (Imply (World x p) (World x q) : delta))
mImplyR _ = Nothing

mImplyL :: Eq a => Rule a
mImplyL (Open s@(Sequent v (World x (MImply p q) : gamma) delta)) =
    Just $ (Rule1 "mImplyL" s (Open $ Sequent v (Imply (World x p) (World x q) : gamma) delta))
mImplyL _ = Nothing

mBoxR :: (Enum a, Eq a) => Rule a
mBoxR (Open s@(Sequent v gamma (World x (MBox p) : delta))) =
    Just (Rule1 "mBoxR" s (Open $ Sequent (newVariable : v) gamma (Forall newVariable (Imply (Accessible x newVariable) (World newVariable p)) : delta))) where
        newVariable = newVar v
mBoxR _ = Nothing

mBoxL :: (Enum a, Eq a) => Rule a
mBoxL (Open s@(Sequent v (World x (MBox p) : gamma) delta)) =
    Just (Rule1 "mBoxL" s (Open $ Sequent (newVariable : v) (Forall newVariable (Imply (Accessible x newVariable) (World newVariable p)) : gamma) delta)) where
        newVariable = newVar v
mBoxL _ = Nothing

mDiamondR :: (Enum a, Eq a) => Rule a
mDiamondR (Open s@(Sequent v gamma (World x (MDiamond p) : delta))) =
    Just (Rule1 "mDiamondR" s (Open $ Sequent (newVariable : v) gamma (Exists newVariable (Imply (Accessible x newVariable) (World newVariable p)) : delta))) where
        newVariable = newVar v
mDiamondR _ = Nothing

mDiamondL :: (Enum a, Eq a) => Rule a
mDiamondL (Open s@(Sequent v (World x (MDiamond p) : gamma) delta)) =
    Just (Rule1 "mDiamondL" s (Open $ Sequent (newVariable : v) (Exists newVariable (Imply (Accessible x newVariable) (World newVariable p)) : gamma) delta)) where
        newVariable = newVar v
mDiamondL _ = Nothing

topR :: (Enum a, Eq a) => Rule a
topR (Open s@(Sequent v gamma (Top : delta))) =
    Just (Rule1 "topR" s Close)
topR _ = Nothing

bottomL :: (Enum a, Eq a) => Rule a
bottomL (Open s@(Sequent v (Bottom : gamma) delta)) =
    Just (Rule1 "bottomL" s Close)
bottomL _ = Nothing

mTopR :: (Enum a, Eq a) => Rule a
mTopR (Open s@(Sequent v gamma (World w (MTop) : delta))) =
    Just (Rule1 "mTopR" s (Open $ Sequent v gamma ((Not $ World w $ MBottom) : delta)))
mTopR _ = Nothing

mTopL :: (Enum a, Eq a) => Rule a
mTopL (Open s@(Sequent v (World w (MTop) : gamma) delta)) =
    Just (Rule1 "mTopL" s (Open $ Sequent v ((Not $ World w $ MBottom) : gamma) delta))
mTopL _ = Nothing

