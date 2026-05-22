import ShowLatex
import Balancing
import Proposition
import Sequent
import ProofTree
--import Rule
--import ProofSearch

main :: IO ()
main = print ""
{-
tree = toProofTree $ Imply
    (Forall 'w' $ Forall 'u' $ Forall 'v' $
        Imply (And (Accessible 'w' 'u') (Accessible 'w' 'v')) (Accessible 'u' 'v'))
    (Forall 't' $ World 't' $
        MImply (MDiamond $ MAtom 'A') (MBox $ MDiamond $ MAtom 'A')
    )
    -}
out :: ShowLatex a => ProofTree a -> IO ()
out = writeFile "proof.tex" . showLatex
