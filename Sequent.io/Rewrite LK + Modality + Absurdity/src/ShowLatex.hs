module ShowLatex where



class ShowLatex a where
    showLatex :: a -> String
    showLatexNonTop :: a -> String

instance ShowLatex String where
    showLatexNonTop = id
    showLatex = id

instance ShowLatex Char where
    showLatexNonTop a = [a]
    showLatex a = [a]


