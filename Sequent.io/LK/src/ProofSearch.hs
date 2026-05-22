module ProofSearch where
import Rule
import Proposition
import Sequent
import ProofTree
import ShowLatex
import Data.List (nub)


branch :: Functor t => t (a -> b) -> a -> t b
branch ls e = fmap (\f -> f e) ls

deflat :: [Maybe a] -> [a]
deflat []             = []
deflat (Nothing : ls) = deflat ls
deflat (Just x : ls)  = x : deflat ls

force :: (Eq a, Enum a) => ProofTree a -> [ProofTree a]
force (Rule1 s p n) = map (\x -> Rule1 s x n) (force p)
force (Rule2 s p q n) = foldr (++) [] $ map (\y -> map (\x -> Rule2 s x y n) (force p)) (force q)
force x = deflat $ branch rules x

prove :: (Eq a, Enum a) => Proposition a -> ProofTree a
prove x = search $ Open $ Sequent (nub $ foldr (:) [] x) [] [x]

search :: (Eq a, Enum a) => ProofTree a -> ProofTree a
search tree
    | isComplete tree = tree
    | otherwise       = aux (force tree) where
        aux :: (Eq a, Enum a) => [ProofTree a] -> ProofTree a
        aux ls
            | filter isComplete ls == [] = aux $ foldr (++) [] $ map force ls
            | otherwise = head $ filter isComplete ls

main = prove $ Imply (Forall 'p' (Atom 'p')) (Atom 'A')
