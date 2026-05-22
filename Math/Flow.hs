data Node = Node Int deriving (Eq, Show)
data Graph = Graph {element :: [Node], relations :: [(Node, Node)]} deriving (Eq, Show)

r :: Int -> Int -> (Node, Node)
r a b = (Node a, Node b)

isRelated :: Graph -> Node -> Node -> Bool
isRelated _ x y | x == y = False
isRelated g x y | (x, y) `elem` relations g = True
isRelated _ _ _ = False

simpleGraph = Graph {
        element = map Node [1..8],
        relations = [
            r 1 2,
            r 1 3,
            r 1 4,
            r 2 5,
            r 2 7,
            r 3 5,
            r 3 6,
            r 4 6,
            r 5 7,
            r 6 8,
            r 7 8
        ]
}

rGraph = Graph {
        element = map Node [1..10],
        relations = [
            r 1 2,
            r 1 3,
            r 1 4,
            r 1 2,
            r 2 5,
            r 2 7,
            r 3 5,
            r 3 6,
            r 4 6,
            r 5 7,
            r 6 8,
            r 7 8,
            r 8 9,
            r 9 10,
            r 1 8
        ]
}

data Path  = Path  Integer deriving (Eq, Ord, Show)
instance Num Path  where
    (+) (Path  a) (Path  b) = Path  (a + b)
    (-) (Path  a) (Path  b) = Path  (a - b)
    (*) (Path  a) (Path  b) = Path  (a * b)
    abs (Path  a) = Path  (abs a)
    signum (Path  a) = Path  (signum a)
    fromInteger a = Path  $ fromInteger a


data MaxDistance = MaxDistance Integer | Inf deriving (Show, Eq)
instance Ord MaxDistance where
    compare (MaxDistance a) (MaxDistance b) = compare a b
    compare Inf (MaxDistance _) = GT
    compare (MaxDistance _) Inf = LT
    compare Inf Inf = EQ
instance Num MaxDistance where
    (+) (MaxDistance a) (MaxDistance b) = MaxDistance (a + b)
    (-) (MaxDistance a) (MaxDistance b) = MaxDistance (a - b)
    (*) (MaxDistance a) (MaxDistance b) = MaxDistance (a * b)
    abs (MaxDistance a) = MaxDistance (abs a)
    signum (MaxDistance a) = MaxDistance (signum a)
    fromInteger a = MaxDistance $ fromInteger a


data Monotonicity = Same | Opposite | NoCorrelation deriving (Show, Eq)

class ForwTravResult a where
    whenNodesEqForwTravResult :: a
    whenNodesRelatedForwTravResult :: a
    unifyForwTravResult :: [a] -> a

class BackTravResult a where
    whenNodesEqBackTravResult :: a
    whenNodesAntiRelatedBackTravResult :: a
    unifyBackTravResult :: [a] -> a


instance BackTravResult Path  where
    whenNodesEqBackTravResult = Path  1
    whenNodesAntiRelatedBackTravResult = Path  0
    unifyBackTravResult = sum

instance ForwTravResult Path  where
    whenNodesEqForwTravResult = Path  1
    whenNodesRelatedForwTravResult = Path  0
    unifyForwTravResult = sum

instance BackTravResult MaxDistance where
    whenNodesEqBackTravResult = MaxDistance 0
    whenNodesAntiRelatedBackTravResult = Inf
    unifyBackTravResult = (+1) . maximum


instance BackTravResult Monotonicity where
    whenNodesEqBackTravResult = Same
    whenNodesAntiRelatedBackTravResult = Opposite
    unifyBackTravResult ls
        | all ((==) Same) ls = Opposite
        | all ((==) Opposite) ls = Same
        | otherwise = NoCorrelation


succSet :: Graph -> Node -> [Node]
succSet graph x = [y | y <- element graph, isRelated graph x y]


predSet :: Graph -> Node -> [Node]
predSet graph x = [y | y <- element graph, isRelated graph y x]


forwTrav :: ForwTravResult a => Graph -> Node -> Node -> a
forwTrav graph x y
    |x == y = whenNodesEqForwTravResult
    |isRelated graph x y = whenNodesRelatedForwTravResult
    |otherwise = unifyForwTravResult $ map (\i -> forwTrav graph i x) (succSet graph y)


backTrav :: BackTravResult a => Graph -> Node -> Node -> a
backTrav graph x y
    |x == y = whenNodesEqBackTravResult
    |isRelated graph y x = whenNodesAntiRelatedBackTravResult
    |otherwise = unifyBackTravResult $ map (backTrav graph x) (predSet graph y)


