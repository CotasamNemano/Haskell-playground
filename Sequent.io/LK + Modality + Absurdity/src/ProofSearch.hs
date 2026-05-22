module ProofSearch where
import Rule
import Proposition
import Sequent
import ProofTree
import Data.List (nub, sortOn)


push :: (Eq a, Enum a) => Rule a -> Rule a
push f Close = Just Close
push f a@(Open _) = case f a of
                         Just a' -> Just a'
                         Nothing -> Just a
push f (Rule1 n s p) = fmap (Rule1 n s) (push f p)
push f (Rule2 n s p q) = case (push f p) of
                              Just p' -> fmap (Rule2 n s p') (push f q)
                              Nothing -> fmap (Rule2 n s p) (push f q)


toProofTree :: Eq a => Proposition a -> ProofTree a
toProofTree x = Open $ Sequent (nub $ foldr (:) [] x) [] [x]


apply 0 f x = x
apply n f x = apply (n - 1) f (f x)


forceWith :: (Eq a, Enum a) => [Rule a] -> ProofTree a -> [ProofTree a]
forceWith r Close = [Close]
forceWith r t@(Open s) = [x | Just x <- map (\f -> f t) r]
forceWith r (Rule1 n s p) = map (\x -> Rule1 n s x) (forceWith r p)
forceWith r (Rule2 n s p q) = foldr (++) [] $ map (\y -> map (\x -> Rule2 n s x y) (forceWith r p)) (forceWith r q)

pRule :: (Eq a, Enum a) => [Rule a]
pRule = [
    (=<<) (push notR) . mNotR, (=<<) (push notL) . mNotL,
    (=<<) (push andR) . mAndR, (=<<) (push andL) . mAndL,
    (=<<) (push orR) . mOrR, (=<<) (push orL) . mOrL,
    (=<<) (push implyR) . mImplyR, (=<<) (push implyL) . mImplyL,
    mBoxR, mBoxL, mDiamondR, mDiamondL
    ]
mRule :: (Eq a, Enum a) => [Rule a]
mRule = [forallL, forallR, existsL, existsR, shuffleL, contractionL, contractionR]
qRule :: (Eq a, Enum a) => [Rule a]
qRule = [identity, notL, notR, andL, andR, orL, orR, implyL, implyR]

xRule :: (Eq a, Enum a) => [Rule a]
xRule = [identity, notL, notR, andL, andR, orL, orR, implyL, implyR,
    (=<<) (push notR) . mNotR, (=<<) (push notL) . mNotL,
    (=<<) (push andR) . mAndR, (=<<) (push andL) . mAndL,
    (=<<) (push orR) . mOrR, (=<<) (push orL) . mOrL,
    (=<<) (push implyR) . mImplyR, (=<<) (push implyL) . mImplyL,
    mBoxR, mBoxL, mDiamondR, mDiamondL
    ]

wRule :: (Eq a, Enum a) => [Rule a]
wRule = [identity, notL, notR, andL, andR, orL, orR, implyL, implyR,
    (=<<) (push notR) . mNotR, (=<<) (push notL) . mNotL,
    (=<<) (push andR) . mAndR, (=<<) (push andL) . mAndL,
    (=<<) (push orR) . mOrR, (=<<) (push orL) . mOrL,
    (=<<) (push implyR) . mImplyR, (=<<) (push implyL) . mImplyL,
    mBoxR, mBoxL, mDiamondR, mDiamondL, forallL, forallR, existsL, existsR, shuffleVar, contractionL, contractionR
    ]

tactic :: (Eq a, Enum a) => ProofTree a -> [ProofTree a]
tactic tree
    | isComplete tree = [tree]
    | ls == []        = forceWith [shuffleL, shuffleR] tree
    | otherwise       = ls
    where
    ls = forceWith xRule tree

tacticWith :: (Eq a, Enum a) => [Rule a] -> ProofTree a -> [ProofTree a]
tacticWith r tree
    | isComplete tree = [tree]
    | ls == []        = forceWith [shuffleL, shuffleR] tree
    | otherwise       = ls
    where
        ls = forceWith r tree

searchWithUntil :: (Eq a, Enum a) => (ProofTree a -> [ProofTree a]) -> (ProofTree a -> Bool) -> ProofTree a -> ProofTree a
searchWithUntil machine pre tree = head $ filter pre $ until (or . map pre) (collect . map machine) [tree]

shuffleVarTo :: (Eq a, Enum a) => a -> ProofTree a -> Maybe (ProofTree a)
shuffleVarTo v tree
    | or $ map ((==) v . head) $ filter (/=[]) $ map (variables . getSequent . fst) $ getLeafLocation tree = Just tree
    | otherwise = push shuffleVar tree >>= shuffleVarTo v

