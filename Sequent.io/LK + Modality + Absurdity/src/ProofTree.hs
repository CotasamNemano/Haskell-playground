module ProofTree where

import Proposition
import Sequent
import ShowLatex
import Balancing
import Data.List (sortOn)

data ProofTree a = Close
                 | Open  (Sequent a)
                 | Rule1 RuleName (Sequent a) (ProofTree a)
                 | Rule2 RuleName (Sequent a) (ProofTree a) (ProofTree a)  deriving (Eq, Show)

data NavTree = LeftTree | RightTree deriving (Eq, Show)
type Location = [NavTree]
type RuleName = String

getSubProofTree :: ProofTree a -> Location -> ProofTree a
getSubProofTree (Rule1 _ _ p ) (_ : locs) = getSubProofTree p locs
getSubProofTree (Rule2 _ _ p _) (LeftTree : locs) = getSubProofTree p locs
getSubProofTree (Rule2 _ _ q _) (RightTree : locs) = getSubProofTree q locs
getSubProofTree t _ = t

getSequent :: ProofTree a -> Sequent a
getSequent (Open s) = s
getSequent (Rule1 _ s _) = s
getSequent (Rule2 _ s _ _) = s
getSequent _ = Sequent [] [] []

isComplete :: ProofTree a -> Bool
isComplete Close = True
isComplete (Open _) = False
isComplete (Rule1 _ _ p) = isComplete p
isComplete (Rule2 _ _ p q) = isComplete p && isComplete q

modify :: (ProofTree a -> ProofTree a) -> Location -> ProofTree a -> ProofTree a
modify f (_ : _)            Close                          = f Close
modify f (_ : _)            p@(Open _)                     = f p
modify f []                 p                              = f p
modify f (_ : locs)         p@(Rule1 n s tree)             = Rule1 n s (modify f locs tree)
modify f (LeftTree : locs)  (Rule2 n s leftTree rightTree) = Rule2 n s (modify f locs leftTree) rightTree
modify f (RightTree : locs) (Rule2 n s leftTree rightTree) = Rule2 n s leftTree                 (modify f locs rightTree)

--getSubProofTree ::
left = \(x, locs) -> (x, LeftTree : locs)
right = \(x, locs) -> (x, RightTree : locs)

getLeafLocation :: ProofTree a -> [(ProofTree a, Location)]
getLeafLocation Close           = [(Close, [])]
getLeafLocation p@(Open _)      = [(p, [])]
getLeafLocation (Rule1 _ _ p)   = left <$> getLeafLocation p
getLeafLocation (Rule2 _ _ p q) = (left <$> getLeafLocation p) ++ (right <$> getLeafLocation q)

reOpen :: ProofTree a -> ProofTree a
reOpen (Rule1 _ s _)   = Open s
reOpen (Rule2 _ s _ _ ) = Open s
reOpen x = x

instance ShowLatex a => ShowLatex (ProofTree a) where
    showLatex t = unlines
        ["\\documentclass{article}"
        ,"\\usepackage{bussproofs}"
        ,"\\usepackage{graphicx}"
        ,"\\usepackage[landscape, left = 0cm, right = 0cm]{geometry}"
        ,"\\usepackage{amsfonts,amssymb}"
        ,"\\begin{document}"
        ,"\\begin{tiny}"
        ,"\\begin{prooftree}"
        , render t
        ,"\\end{prooftree}"
        ,"\\end{tiny}"
        ,"\\end{document}"
        ] where
            render :: ShowLatex a => ProofTree a -> String
            render Close =
                "\\AxiomC{$\\cdot$}"  -- or empty placeholder
            render (Open s) =
                "\\AxiomC{$" ++ showLatex s ++ "$}"
            render (Rule1 rule s p) = unlines
                [ render p
                , "\\RightLabel{\\scriptsize " ++ escape rule ++ "}"
                , "\\UnaryInfC{$" ++ showLatex s ++ "$}"
                ]
            render (Rule2 rule s p q) = unlines
                [ render p
                , render q
                , "\\RightLabel{\\scriptsize " ++ escape rule ++ "}"
                , "\\BinaryInfC{$" ++ showLatex s ++ "$}"
                ]
            escape :: String -> String
            escape = concatMap (\c -> if c == '_' then "\\_" else [c])
    showLatexNonTop = showLatex
pretty [] = ""
pretty (x : xs) = show x ++ "\n" ++ show xs

instance Weight a => Weight (ProofTree a) where
    weight Close = 0
    weight (Open s) = weight s
    weight (Rule1 n s p) = weight s + weight p + 1
    weight (Rule2 n s p q) = weight s + weight p + weight q + 2

instance Weight a => Center (ProofTree a) where
    center Close = 0
    center (Open s) = 0
    center (Rule1 n s p) = center p
    center (Rule2 n s p q) = (center p - 2) * weight p + (center q + 2) * weight q

perm :: ProofTree a -> [ProofTree a]
perm Close = [Close]
perm (Open s) = [Open s]
perm (Rule1 n s p) = map (\x -> Rule1 n s x) (perm p)
perm (Rule2 n s p q) = collect (map (\y -> map (\x -> Rule2 n s x y) (perm p)) (perm q)) ++
                       collect (map (\y -> map (\x -> Rule2 n s x y) (perm q)) (perm p))

collect :: [[a]] -> [a]
collect = foldr (++) []

reCenter :: Weight a => ProofTree a -> ProofTree a
reCenter = fst . head . sortOn (abs . snd) . map (\t -> (t, center t)) . perm

instance PropositionProperty (ProofTree a) where
    haveModality = or . map (haveModality . getSequent . fst) . getLeafLocation
    haveQuantifier = or . map (haveQuantifier . getSequent . fst) . getLeafLocation
    isClassical = and . map (isClassical . getSequent . fst) . getLeafLocation

