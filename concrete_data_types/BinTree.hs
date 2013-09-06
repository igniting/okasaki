data BinTree a = Empty
                |NodeBT a (BinTree a) (BinTree a)
                deriving Show
-- Traversing the tree
tSum :: BinTree Int -> Int
tSum Empty = 0
tSum (NodeBT n lt rt) = n + tSum lt + tSum rt
