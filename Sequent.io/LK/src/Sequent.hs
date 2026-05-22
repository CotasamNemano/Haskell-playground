module Sequent where

import Proposition
import ShowLatex

data Sequent a = Sequent [a] [Proposition a] [Proposition a] deriving (Eq, Show)

instance Functor Sequent where
    fmap f (Sequent var antecedent consequent) =
        Sequent (fmap f var)
                (map (fmap f) antecedent)
                (map (fmap f) consequent)

instance ShowLatex a => ShowLatex (Sequent a) where
    showLatex (Sequent _ p q) = showLatex p ++ " \\vdash " ++ showLatex q


instance ShowLatex a => ShowLatex [Proposition a] where
    showLatex (x : y : xs) = showLatex x ++ "," ++ showLatex (y : xs)
    showLatex (x : [])     = showLatex x
    showLatex [] = ""
