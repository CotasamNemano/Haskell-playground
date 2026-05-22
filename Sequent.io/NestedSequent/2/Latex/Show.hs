module Latex.Show where

class LatexShow a where
    latexShow :: a -> String
    latexShowNonTop :: a -> String
    latexShowNonTop a = "(" ++ latexShow a ++ ")"

instance LatexShow String where
    latexShowNonTop = id
    latexShow = id

instance LatexShow Char where
    latexShowNonTop a = [a]
    latexShow a = [a]


