{-# LANGUAGE OverloadedStrings #-}
import Data.String (IsString, fromString)
import Data.Default (Default, def)
import Data.List (sort)

class Formulaic a where
    negation :: a -> a

class ToFormula a where
    toFormula :: a -> Formula

class ToSequent a where
    toSequent :: a -> Sequent

class Depth a where
    depth :: a -> Int

data Atom = Pure String
          | Bar String
          deriving (Eq, Ord, Show)

instance IsString Atom where
    fromString = Pure

instance Default Atom where
    def = "a"

instance Formulaic Atom where
    negation (Pure a) = Bar a
    negation (Bar a) = Pure a

instance ToFormula Atom where
    toFormula = AtomP

instance ToSequent Atom where
    toSequent = SFor . AtomP

data Formula = AtomP Atom
             | Or Formula Formula
             | And Formula Formula
             | Box Formula
             | Diamond Formula
             deriving (Ord, Show)

instance Eq Formula where
    AtomP a == AtomP b = a == b
    Or a b == Or c d | a == negation b && c == negation d = True
                     | otherwise = a == c && b == d
    And a b == And c d | a == negation b && c == negation d = True
                       | otherwise = a == c && b == d
    Box a == Box b = a == b
    Diamond a == Diamond b = a == b
    _ == _ = False

instance Default Formula where
    def = AtomP "a"

instance IsString Formula where
    fromString = AtomP . Pure

instance Formulaic Formula where
    negation (AtomP a) = AtomP (negation a)
    negation (Or a b) = And (negation a) (negation b)
    negation (And a b) = Or (negation a) (negation b)
    negation (Box a) = Diamond (negation a)
    negation (Diamond a) = Box (negation a)

instance ToFormula Formula where
    toFormula = id

top = Or def (negation def)

bottom = And def (negation def)

imply a b = Or (negation a) b

data Sequent = SFor Formula
             | SBox Sequent
             | SSeq [Sequent]
            deriving (Show, Eq, Ord)

instance ToFormula Sequent where
    toFormula (SFor f) = f
    toFormula (SBox s) = Box $ toFormula s
    toFormula (SSeq s) = foldr Or bottom $ map toFormula s

data Context = Hole Int
             | CFor Formula
             | CBox Context
             | CSeq [Context]
             deriving (Eq, Show)

fill :: Context -> [Sequent] -> Sequent
fill (Hole i) ls = ls !! i
fill (CFor f) _  = SFor f
fill (CBox c) ls = SBox $ fill c ls
fill (CSeq m) ls = SSeq $ map (\x -> fill x ls) m

instance Depth Context where
    depth (Hole _) = 0
    depth (CFor _) = 0
    depth (CBox c) = depth c + 1
    depth (CSeq ls) = foldr max 0 $ map depth ls

type Rule = String

data Tree = Close
          | Open Sequent
          | UnaryTree Rule Sequent Tree
          | BinaryTree Rule Sequent Tree Tree
          deriving (Show)

axiom :: Tree -> Context -> Maybe Tree
axiom (Open s) c | s == fill c [SFor top] = Just $ UnaryTree "Axiom" s Close
                 | otherwise = Nothing
axiom _ _ = Nothing
