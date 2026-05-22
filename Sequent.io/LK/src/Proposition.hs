{-# LANGUAGE DeriveFoldable #-}
module Proposition where
import ShowLatex

data Proposition a  = Atom   a
                    | And    (Proposition a) (Proposition a)
                    | Or     (Proposition a) (Proposition a)
                    | Imply  (Proposition a) (Proposition a)
                    | Not    (Proposition a)
                    | Forall a              (Proposition a)
                    | Exists a              (Proposition a)
                    deriving (Eq, Show, Foldable)

substitute :: Eq a => a -> a -> Proposition a -> Proposition a
substitute x y = fmap (\v -> case v of x -> y)

instance Functor Proposition where
    fmap f (Atom   s)   = Atom   (f s)
    fmap f (And    p q) = And    (fmap f p) (fmap f q)
    fmap f (Or     p q) = Or     (fmap f p) (fmap f q)
    fmap f (Imply  p q) = Imply  (fmap f p) (fmap f q)
    fmap f (Forall s p) = Forall (f s)      (fmap f p)
    fmap f (Exists s p) = Exists (f s)      (fmap f p)

instance Traversable Proposition where
    traverse f (Atom   s)   = Atom   <$> f s
    traverse f (And    p q) = And    <$> traverse f p <*> traverse f q
    traverse f (Or     p q) = Or     <$> traverse f p <*> traverse f q
    traverse f (Imply  p q) = Imply  <$> traverse f p <*> traverse f q
    traverse f (Not    p)   = Not    <$> traverse f p
    traverse f (Forall s p) = Forall <$> f s          <*> traverse f p
    traverse f (Exists s p) = Exists <$> f s          <*> traverse f p


instance ShowLatex a => ShowLatex (Proposition a) where
    showLatex (Atom a) = showLatex a
    showLatex (And a b) = "(" ++ showLatex a ++ " \\land " ++ showLatex b ++ ")"
    showLatex (Or a b) = "(" ++ showLatex a ++ " \\lor " ++ showLatex b ++ ")"
    showLatex (Imply a b) = "(" ++ showLatex a ++ " \\rightarrow " ++ showLatex b ++ ")"
    showLatex (Not a) = "\\lnot" ++ showLatex a
    showLatex (Forall a b) = "\\forall " ++ showLatex a ++ "." ++ showLatex b
    showLatex (Exists a b) = "\\exists " ++ showLatex a ++ "." ++ showLatex b
