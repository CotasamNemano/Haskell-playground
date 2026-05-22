{-# LANGUAGE DeriveFoldable #-}
module Proposition where
import ShowLatex
import Balancing

data Proposition a  = Atom      a
                    | And       (Proposition a) (Proposition a)
                    | Or        (Proposition a) (Proposition a)
                    | Imply     (Proposition a) (Proposition a)
                    | Not       (Proposition a)
                    | Forall    a               (Proposition a)
                    | Exists    a               (Proposition a)
                    | World     a               (ModalProposition a)
                    | Accessible a a
                    deriving (Eq, Show, Foldable)
data ModalProposition a = MAtom a
                        | MAnd     (ModalProposition a) (ModalProposition a)
                        | MOr      (ModalProposition a) (ModalProposition a)
                        | MImply   (ModalProposition a) (ModalProposition a)
                        | MNot     (ModalProposition a)
                        | MBox     (ModalProposition a)
                        | MDiamond (ModalProposition a)
                        deriving (Eq, Show, Foldable)

substitute :: Eq a => a -> a -> Proposition a -> Proposition a
substitute x y = fmap (\v -> if v == x then y else v)

instance Functor Proposition where
    fmap f (Atom      s)   = Atom      (f s)
    fmap f (And       p q) = And       (fmap f p) (fmap f q)
    fmap f (Or        p q) = Or        (fmap f p) (fmap f q)
    fmap f (Imply     p q) = Imply     (fmap f p) (fmap f q)
    fmap f (Not       p)   = Not       (fmap f p)
    fmap f (Forall    s p) = Forall    (f s)      (fmap f p)
    fmap f (Exists    s p) = Exists    (f s)      (fmap f p)
    fmap f (World     s p) = World     (f s)      (fmap f p)
    fmap f (Accessible s t) = Accessible (f s) (f t)


instance Traversable Proposition where
    traverse f (Atom      s)   = Atom      <$> f s
    traverse f (And       p q) = And       <$> traverse f p <*> traverse f q
    traverse f (Or        p q) = Or        <$> traverse f p <*> traverse f q
    traverse f (Imply     p q) = Imply     <$> traverse f p <*> traverse f q
    traverse f (Not       p)   = Not       <$> traverse f p
    traverse f (Forall    s p) = Forall    <$> f s          <*> traverse f p
    traverse f (Exists    s p) = Exists    <$> f s          <*> traverse f p
    traverse f (Accessible s p) = Accessible <$> f s          <*> f p


instance ShowLatex a => ShowLatex (Proposition a) where
    showLatexNonTop (Atom a) = showLatex a
    showLatexNonTop a = "(" ++ showLatex a ++ ")"
    showLatex (Atom a) = showLatex a
    showLatex (And a b) = showLatexNonTop a ++ " \\land " ++ showLatexNonTop b
    showLatex (Or a b) = showLatexNonTop a ++ " \\lor " ++ showLatexNonTop b
    showLatex (Imply a b) = showLatexNonTop a ++ " \\rightarrow " ++ showLatexNonTop b
    showLatex (Not a) = "\\lnot" ++ showLatexNonTop a
    showLatex (Forall a b) = "\\forall " ++ showLatexNonTop a ++ "." ++ showLatexNonTop b
    showLatex (Exists a b) = "\\exists " ++ showLatexNonTop a ++ "." ++ showLatexNonTop b
    showLatex (World a b) = showLatexNonTop a ++ "\\Vdash " ++ showLatex b
    showLatex (Accessible a b) = showLatex a ++ "\\mathcal{R} " ++ showLatex b



instance Functor ModalProposition where
    fmap f (MAtom    s)   = MAtom    (f s)
    fmap f (MAnd     p q) = MAnd     (fmap f p) (fmap f q)
    fmap f (MOr      p q) = MOr      (fmap f p) (fmap f q)
    fmap f (MImply   p q) = MImply   (fmap f p) (fmap f q)
    fmap f (MNot     p)   = MNot     (fmap f p)
    fmap f (MBox     p)   = MBox     (fmap f p)
    fmap f (MDiamond p)   = MDiamond (fmap f p)

instance ShowLatex a => ShowLatex (ModalProposition a) where
    showLatexNonTop (MAtom a) = showLatex a
    showLatexNonTop a = "(" ++ showLatex a ++ ")"
    showLatex (MAtom a) = showLatex a
    showLatex (MAnd a b) = showLatexNonTop a ++ " \\land " ++ showLatexNonTop b
    showLatex (MOr a b) = showLatexNonTop a ++ " \\lor " ++ showLatexNonTop b
    showLatex (MImply a b) = showLatexNonTop a ++ " \\rightarrow " ++ showLatexNonTop b
    showLatex (MNot a) = "\\lnot" ++ showLatexNonTop a
    showLatex (MBox a) = "\\Box " ++ showLatexNonTop a
    showLatex (MDiamond a) = "\\Diamond " ++ showLatexNonTop a

instance Weight a => Weight (ModalProposition a) where
    weight (MAtom a) = weight a
    weight (MNot a) = weight a + 1
    weight (MAnd a b) = weight a + weight b + 1
    weight (MOr a b) = weight a + weight b + 1
    weight (MImply a b) = weight a + weight b + 1
    weight (MBox a) = weight a + 1
    weight (MDiamond a) = weight a + 1


instance Weight a => Weight (Proposition a) where
    weight (Atom a) = weight a
    weight (Not a) = weight a + 1
    weight (And a b) = weight a + weight b + 1
    weight (Or a b) = weight a + weight b + 1
    weight (Imply a b) = weight a + weight b + 1
    weight (Forall a b) = weight a + weight b + 2
    weight (Exists a b) = weight a + weight b + 2
    weight (World a b) = weight a + weight b + 1
    weight (Accessible a b) = weight a + weight b + 1

class PropositionProperty a where
    haveModality :: a -> Bool
    haveQuantifier :: a -> Bool
    isClassical :: a -> Bool

instance PropositionProperty (Proposition a) where
    haveModality (World _ (MAtom _)) = False
    haveModality (World _ _) = True
    haveModality _ = False

    haveQuantifier (Forall _ _) = True
    haveQuantifier (Exists _ _) = True
    haveQuantifier _ = False

    isClassical (Forall _ _) = False
    isClassical (Exists _ _) = False
    isClassical (World _ (MAtom _)) = True
    isClassical (World _ _) = False
    isClassical _ = True

