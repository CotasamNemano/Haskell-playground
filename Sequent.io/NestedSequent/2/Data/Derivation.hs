module Data.Derivation where
import Data.Context
import Data.Either3

------------------------ Derivation ------------------------
type RuleName = String

data Derivation where
  Tree'  :: NestedSequent -> RuleName -> [Derivation] -> Derivation
  Close' :: NestedSequent -> RuleName -> Derivation
  deriving (Eq, Ord, Show)

data IncompleteDerivation where
  Tree  :: NestedSequent -> RuleName -> [IncompleteDerivation] -> IncompleteDerivation
  Close :: NestedSequent -> RuleName -> IncompleteDerivation
  Open  :: NestedSequent -> IncompleteDerivation
  deriving (Eq, Ord, Show)

complete :: IncompleteDerivation -> Maybe Derivation
complete (Tree s r ds) = do
  ds' <- mapM complete ds
  return $ Tree' s r ds'
complete (Close s r) = return $ Close' s r
complete (Open _) = fail "Tree has open branches"

incomplete :: Derivation -> IncompleteDerivation
incomplete (Tree' s r ds) = Tree s r $ fmap incomplete ds
incomplete (Close' s r)   = Close s r

------------------------ Rule ------------------------
axiom :: Context -> Formula -> Maybe IncompleteDerivation
axiom ctx f = do
  let p = toContext f
  let notP = toContext $ negation f
  sequent <- contextToMaybeNestedSequent $ fill ctx 0 [p, notP]
  return $ Close sequent "axiom"

ruleAnd :: Context -> Formula -> Formula -> Maybe IncompleteDerivation
ruleAnd ctx fA fB = do
  seqAB <- contextToMaybeNestedSequent $ fill ctx 0 [toContext $ And fA fB]
  seqA  <- contextToMaybeNestedSequent $ fill ctx 0 [toContext fA]
  seqB  <- contextToMaybeNestedSequent $ fill ctx 0 [toContext fB]
  return $ Tree seqAB "and" [Open seqA, Open seqB]

ruleOr :: Context -> Formula -> Formula -> Maybe IncompleteDerivation
ruleOr ctx fA fB = do
  seqAB <- contextToMaybeNestedSequent $ fill ctx 0 [toContext $ And fA fB]
  seqU  <- contextToMaybeNestedSequent $ fill ctx 0 [toContext fA, toContext fB]
  return $ Tree seqAB "or" [Open seqU]

ruleBox :: Context -> Formula -> Maybe IncompleteDerivation
ruleBox ctx fA = do
  seqD  <- contextToMaybeNestedSequent $ fill ctx 0 [toContext $ Box fA]
  seqU  <- contextToMaybeNestedSequent $ fill ctx 0 [wrap $ toContext fA]
  return $ Tree seqD "box" [Open seqU]

rulekc :: Context -> NestedSequent -> Formula -> Maybe IncompleteDerivation
rulekc ctx s fA = do
  seqD  <- contextToMaybeNestedSequent $ fill ctx 0 [toContext $ NestedSequent [Left $ Diamond fA, Right $ NestedSequent [Right s]]]
  seqU  <- contextToMaybeNestedSequent $ fill ctx 0 [toContext $ NestedSequent [Left $ Diamond fA, Right $ NestedSequent [Right s, Left fA]]]
  return $ Tree seqD "kc" [Open seqU]

ruledc :: Context -> Formula -> Maybe IncompleteDerivation
ruledc ctx fA = do
  seqD <- contextToMaybeNestedSequent $ fill ctx 0 [toContext $ Diamond fA]
  seqU <- contextToMaybeNestedSequent $ fill ctx 0 [toContext $ NestedSequent [Left $ Diamond fA, Right $ toNestedSequent fA]]
  return $ Tree seqD "dc" [Open seqU]