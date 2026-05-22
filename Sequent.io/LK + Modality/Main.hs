import Rule
import Proposition
import Sequent
import ProofTree
import ShowLatex
import Balancing
import ProofSearch

main :: IO ()
main = print ""

tree = toProofTree $ Imply
    (Forall 'w' $ Forall 'v' $ Forall 'u' $
        Imply (And (Accessible 'w' 'v') (Accessible 'v' 'u')) (Accessible 'w' 'u'))
    (Forall 't' $ World 't' $
        MImply (MBox $ MAtom 'A') (MBox $ MBox $ MAtom 'A')
    )

out :: ShowLatex a => ProofTree a -> IO ()
out = writeFile "proof.tex" . showLatex
