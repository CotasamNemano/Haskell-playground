module Sequent where

import Proposition
import ShowLatex
import Balancing

data Sequent a = Sequent {
    antecedent :: [Proposition a],
    consequent :: [Proposition a]
    } deriving (Eq, Show)

(|-) = Sequent
infixr 6 |-

instance Functor Sequent where
    fmap f (Sequent a c) =
        Sequent (fmap (fmap f) a) (fmap (fmap f) c)

instance ShowLatex a => ShowLatex (Sequent a) where
    showLatex (Sequent  p q) = showLatex p ++ " \\vdash " ++ showLatex q
    showLatexNonTop = showLatex


instance ShowLatex a => ShowLatex [Propositionkln a] where
    showLatex (x : y : xs) = showLatex x ++ "," ++ showLatex (y : xs)
    showLatex (x : [])     = showLatex x
    showLatex [] = ""
    showLatexNonTop = showLatex

instance Weight a => Weight (Sequent a) where
    weight (Sequent  a b) = weight a + weight b + 1

instance PropositionProperty (Sequent a) where
    haveModality (Sequent  a b) = or $ map haveModality (a ++ b)
    haveQuantifier (Sequent  a b) = or $ map haveQuantifier (a ++ b)
    isClassical (Sequent  a b) = and $ map isClassical (a ++ b)
