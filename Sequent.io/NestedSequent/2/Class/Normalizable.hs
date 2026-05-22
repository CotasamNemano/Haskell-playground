module Class.Normalizable where

class Normalizable a where
    normalize :: a -> a
    
    (~~) :: Eq a => a -> a -> Bool
    (~~) x y = normalize x == normalize y 
    infix 4 ~~ 