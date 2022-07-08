data Tree a = Tree a (Maybe (Tree a)) (Maybe (Tree a))
  deriving Show