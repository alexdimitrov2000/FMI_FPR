main :: IO()
main = do
    print (t1)
    
    print (size t1)
    
    print (height t1)
    
    print (sumTree t1)
    
    print (sumLeaves t1)
    
    print (inorder t1)
    print (inorder t2)
    
    print (getLevel 1 t2)
    print (getLevel 2 t2)
    print (getLevel 3 t2)

data BTree = Empty | Node Int BTree BTree
    deriving Show

t1 :: BTree                                    --    5
t1 = Node 5 (Node 2 Empty                      --   / \
                    (Node 3 Empty Empty))      --  2   6
            (Node 6 Empty Empty)               --   \
                                               --    3
-- inorder t1 -> [2, 3, 5, 6]

t2 :: BTree                                    --      5
t2 = Node 5 (Node 2 (Node 1 Empty Empty)       --    /   \
                    (Node 3 Empty Empty))      --   2     7
            (Node 7 (Node 6 Empty Empty)       --  / \   / \
                    (Node 8 Empty Empty))      -- 1   3 6   8
-- inorder t2 -> [1, 2, 3, 5, 6, 7, 8]


size :: BTree -> Int
size Empty = 0
size (Node _ lt rt) = 1 + size lt + size rt

height :: BTree -> Int
height Empty = 0
height (Node _ lt rt) = 1 + max (height lt) (height rt)

sumTree :: BTree -> Int
sumTree Empty = 0
sumTree (Node v lt rt) = v + sumTree lt + sumTree rt

sumLeaves :: BTree -> Int
sumLeaves Empty = 0
sumLeaves (Node v Empty Empty) = v
sumLeaves (Node _ lt    rt) = sumLeaves lt + sumLeaves rt

inorder :: BTree -> [Int]
inorder Empty = []
inorder (Node v lt rt) = inorder lt ++ [v] ++ inorder rt

getLevel :: Int -> BTree -> [Int]
getLevel _ Empty = []
getLevel 1 (Node v _ _) = [v]
getLevel k (Node _ lt rt) = getLevel (k - 1) lt ++ getLevel (k - 1) rt