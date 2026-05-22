module ProofTree where

import Sequent
import ShowLatex

data ProofTree a = Close
                 | Open  (Sequent a)
                 | Rule1 (Sequent a) (ProofTree a) RuleName
                 | Rule2 (Sequent a) (ProofTree a) (ProofTree a) RuleName deriving (Eq, Show)

data NavTree = LeftTree | RightTree
type Location = [NavTree]
type RuleName = String

isComplete :: ProofTree a -> Bool
isComplete Close = True
isComplete (Open _) = False
isComplete (Rule1 _ p _) = isComplete p
isComplete (Rule2 _ p q _) = isComplete p && isComplete q

modify :: (ProofTree a -> ProofTree a) -> Location -> ProofTree a -> ProofTree a
modify f (_ : _)            Close                          = f Close
modify f (_ : _)            p@(Open _)                     = f p
modify f (_ : locs)         p@(Rule1 s tree n)             = Rule1 s (modify f locs tree)                               n
modify f (LeftTree : locs)  (Rule2 s leftTree rightTree n) = Rule2 s (modify f locs leftTree) rightTree                 n
modify f (RightTree : locs) (Rule2 s leftTree rightTree n) = Rule2 s leftTree                 (modify f locs rightTree) n

--getSubProofTree ::
left = \(x, locs) -> (x, LeftTree : locs)
right = \(x, locs) -> (x, RightTree : locs)

getLeafLocation :: ProofTree a -> [(ProofTree a, Location)]
getLeafLocation Close           = [(Close, [])]
getLeafLocation p@(Open _)      = [(p, [])]
getLeafLocation (Rule1 _ p _)   = left <$> getLeafLocation p
getLeafLocation (Rule2 _ p q _) = (left <$> getLeafLocation p) ++ (right <$> getLeafLocation q)

reOpen :: ProofTree a -> ProofTree a
reOpen (Rule1 s _ _)   = Open s
reOpen (Rule2 s _ _ _) = Open s
reOpen x = x

instance ShowLatex a => ShowLatex (ProofTree a) where
    showLatex t = unlines
        ["\\documentclass{article}"
        ,"\\usepackage{bussproofs}"
        ,"\\usepackage{graphicx}"
        ,"\\begin{document}"
        ,"\\begin{prooftree}"
        , render t
        , "\\end{prooftree}"
        ,"\\end{document}"] where

            render :: ShowLatex a => ProofTree a -> String
            render Close =
                "\\AxiomC{$\\cdot$}"  -- or empty placeholder
            render (Open seq) =
                "\\AxiomC{$" ++ showLatex seq ++ "$}"
            render (Rule1 seq p rule) = unlines
                [ render p
                , "\\RightLabel{\\scriptsize " ++ escape rule ++ "}"
                , "\\UnaryInfC{$" ++ showLatex seq ++ "$}"
                ]
            render (Rule2 seq p q rule) = unlines
                [ render p
                , render q
                , "\\RightLabel{\\scriptsize " ++ escape rule ++ "}"
                , "\\BinaryInfC{$" ++ showLatex seq ++ "$}"
                ]
            escape :: String -> String
            escape = concatMap (\c -> if c == '_' then "\\_" else [c])


