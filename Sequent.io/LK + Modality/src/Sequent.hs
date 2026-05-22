module Sequent where

import Proposition
import ShowLatex
import Balancing

data Sequent a = Sequent {
    variables :: [a],
    antecedent :: [Proposition a],
    consequent :: [Proposition a]
    } deriving (Eq, Show)

instance Functor Sequent where
    fmap f (Sequent var a c) =
        Sequent (fmap f var)
                (map (fmap f) a)
                (map (fmap f) c)

instance ShowLatex a => ShowLatex (Sequent a) where
    showLatex (Sequent _ p q) = showLatex p ++ " \\vdash " ++ showLatex q
    showLatexNonTop = showLatex


instance ShowLatex a => ShowLatex [Proposition a] where
    showLatex (x : y : xs) = showLatex x ++ "," ++ showLatex (y : xs)
    showLatex (x : [])     = showLatex x
    showLatex [] = ""
    showLatexNonTop = showLatex

instance Weight a => Weight (Sequent a) where
    weight (Sequent _ a b) = weight a + weight b + 1

instance PropositionProperty (Sequent a) where
    haveModality (Sequent _ a b) = or $ map haveModality (a ++ b)
    haveQuantifier (Sequent _ a b) = or $ map haveQuantifier (a ++ b)
    isClassical (Sequent _ a b) = and $ map isClassical (a ++ b)
