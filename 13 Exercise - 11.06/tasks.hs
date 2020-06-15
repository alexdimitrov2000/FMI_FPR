import Data.List

main :: IO()
main = do
    print (twoChildrenNodes t1)
    print (allHaveTwoChildren t1)
    print (allHaveTwoChildren t2)
    print (findUncles t3 5)
    print (findUncles t3 1)
    print (findUncles t3 7)

t1 :: [(Int, [Int])]
t1 = [(4, [2, 5]), (2, [1, 3]), (3, [5])]

t2 :: [(Int, [Int])]
t2 = [(4, [2, 5]), (2, [1, 3]), (3, [5, 6])]

-- Task 01
twoChildrenNodes :: [(Int, [Int])] -> Int
twoChildrenNodes ts = length [v | (v, vs) <- ts, length vs == 2]

-- Task 02
allHaveTwoChildren :: [(Int, [Int])] -> Bool
allHaveTwoChildren ts = and [length vs == 2 | (_, vs) <- ts]

-- Task 03
t3 = [(1,[2,3,4]),(2,[5,6]),(3,[7]),(4,[8,9]),(5,[]),(6,[10]),(7,[]),(8,[]),(9,[]),(10,[])]

findUncles :: [(Int, [Int])] -> Int -> [Int]
findUncles ts v
    | parent == [] = []
    | otherwise = brothers (head parent)
        where
            parent = [u | (u, us) <- ts, elem v us]
            brothers p = concat [delete p bs | (_, bs) <- ts, elem p bs]

-- findUncles ts v = if null parent then [] else brothers (head parent)
--     where
--         parent = [u | (u, us) <- ts, elem v us]
--         brothers p = concat [delete p bs | (_, bs) <- ts, elem p bs]