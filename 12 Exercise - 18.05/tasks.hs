main :: IO()
main = do
    print (size t1)
    print (height t1)
    print (sumTree t1)
    print (sumLeaves t1)
    print (inorder t1)

    print (getLevel 3 t1)
    print (getLevel 4 t1)
    
    print (average t1)
    -- print (average1 t1)

    print (mirrorTree t1)
    print (getLevelsTree t1)

    print (maxDepthBlueNode colorTree)

    print (containsWord testTree "bew")
    print (containsWord testTree "abe")

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

t1 :: BTree Int                                             --     1                  (1,0)
t1 = Node 1 (Node 2 (Node 5 Empty (Node 4 Empty Empty))     --    / \   levels        /    \
                  Empty)                                    --   2   3    =>      (2,1)   (3,1)
                     (Node 3 (Node 7 Empty Empty)           --  /   / \            /      /   \
                             (Node 6 Empty Empty))          -- 5   7   6       (5,2)   (7,2)  (6,2)
                                                            --  \                \
                                                            --   4              (4,3)

size :: BTree a -> Int
size Empty = 0
size (Node _ lt rt) = 1 + size lt + size rt

height :: BTree a -> Int
height Empty = 0
height (Node _ lt rt) = 1 + max (height lt) (height rt)

-- Eq ==
-- Ord <, >, ...
-- Num +, -, ...

sumTree :: Num a => BTree a -> a
sumTree Empty = 0
sumTree (Node v lt rt) = v + sumTree lt + sumTree rt

sumLeaves :: Num a => BTree a -> a
sumLeaves Empty = 0
sumLeaves (Node v Empty Empty) = v
sumLeaves (Node _ lt rt) = sumLeaves lt + sumLeaves rt

inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node v lt rt) = inorder lt ++ [v] ++ inorder rt

getLevel :: Int -> BTree a -> [a]
getLevel _ Empty = []
getLevel 1 (Node v _ _) = [v]
getLevel k (Node _ lt rt) = getLevel (k - 1) lt ++ getLevel (k - 1) rt

average :: BTree Int -> Double
average tree = fromIntegral (sumTree tree) / fromIntegral (size tree)

-- average1 :: Num a => BTree a -> Double
-- average1 tree = fromIntegral (sum vs) / fromIntegral (size vs)
--     where
--         vs = inorder tree

mirrorTree :: BTree a -> BTree a
mirrorTree Empty = Empty
mirrorTree (Node v lt rt) = Node v (mirrorTree rt) (mirrorTree lt)

isEqual :: Eq a => BTree a -> BTree a -> Bool
isEqual Empty Empty = True
isEqual _ Empty = False
isEqual Empty _ = False
isEqual (Node v1 lt1 rt1) (Node v2 lt2 rt2) = (v1 == v2) && (isEqual lt1 lt2) && (isEqual rt1 rt2)

getLevelsTree :: BTree a -> BTree (a, Int)
getLevelsTree Empty = Empty
getLevelsTree tree = helper tree 0
    where
        helper Empty _ = Empty
        helper (Node a lt rt) level = (Node (a, level) (helper lt (level + 1)) (helper rt (level + 1)))

data Color = Red | Green | Blue deriving (Read, Show, Eq)

colorTree :: BTree Color                                                --            Blue
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty)         --           /    \
                      (Node Red (Node Blue (Node Green Empty Empty)     --        Red     Red
                                            (Node Blue Empty Empty))     --        /        /  
                                Empty)                                  --     Green     Blue  
                                                                        --              /   \
                                                                        --           Green  Blue

maxDepthBlueNode :: BTree Color -> Int
maxDepthBlueNode Empty = -1
maxDepthBlueNode (Node Blue lt rt) = max 0 (1 + max (maxDepthBlueNode lt) (maxDepthBlueNode rt))
maxDepthBlueNode (Node _ lt rt) = 1 + max (maxDepthBlueNode lt) (maxDepthBlueNode rt)

testTree :: BTree Char                                        
testTree = Node 'a' (Node 'c' (Node 'f' Empty Empty)          
                        (Node 'd' Empty Empty))         
              (Node 'b' Empty                           
                    (Node 'e' (Node 'w' Empty Empty) (Node 's' Empty Empty)))

-- testTree:
--     a
--    / \
--   c   b
--  / \   \
-- f   d   e 
--        / \
--       w   s

getFullPaths :: BTree Char -> [String]
getFullPaths Empty = []
getFullPaths (Node v Empty Empty) = [[v]]
getFullPaths (Node v lt rt) = [v : str | str <- path]
    where
        path = getFullPaths lt ++ getFullPaths rt

containsWord :: BTree Char -> String -> Bool
containsWord tree str = elem str (map (reverse . take (length str) . reverse) (getFullPaths tree))