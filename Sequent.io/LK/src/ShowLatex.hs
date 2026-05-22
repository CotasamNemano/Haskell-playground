module ShowLatex where

class ShowLatex a where
    showLatex :: a -> String

instance ShowLatex String where
    showLatex = id

instance ShowLatex Char where
    showLatex = show
