main :: IO()
main = do
    print ("Task 01")
    print (multiplyAllBy [1, 2, 3] 5)

    print ("Task 02")
    print (filterSmallerThan [1, 5, 20] 15)

    print ("Task 03")
    print (isAscending 12345)
    print (isAscending 123745)
    
    print ("Task 04")
    print (isImage [1, 2, 3] [2, 3, 4])
    print (isImage [5, 2, 3] [2, 3, 4])

    print ("Task 05")
    print (chunksOf 2 [1, 2, 3, 4, 5, 6, 7, 8, 9])
    print (chunksOf 3 [1, 2, 3, 4, 5, 6, 7, 8, 9])
    print (chunksOf 4 [1, 2, 3, 4, 5, 6, 7, 8, 9])
    print (chunksOf 0 [1, 2, 3, 4, 5, 6, 7, 8, 9])

    print ("Task 06")
    print (divisors 20)
    print (divisors 30)

    print ("Task 07")
    print (isTriangular [[1, 2, 3], [5, 6, 7], [8, 9, 0]])
    print (isTriangular [[1, 2, 3], 
                         [0, 6, 7], 
                         [0, 0, 9]])
    
    print (map head [[1,2,3], [5,6,7], [8,9,0]])
    print (map tail [[1,2,3], [5,6,7], [8,9,0]])

-- Task 01
multiplyAllBy :: [Int] -> Int -> [Int]
multiplyAllBy xs n = map (* n) xs
-- multiplyAllBy xs n = [x * n | x <- xs]

-- Task 02
filterSmallerThan :: [Int] -> Int -> [Int]
filterSmallerThan xs n = filter (< n) xs
-- filterSmallerThan xs n = [x | x <- xs, x < n]

-- Task 03
convertIntToList :: Int -> [Int]
convertIntToList 0 = []
convertIntToList n =  (n `mod` 10) : (convertIntToList (n `div` 10))

isAscending :: Int -> Bool
isAscending n = helper (reverse (convertIntToList n))
    where
        helper ns = and [a <= b | (a, b) <- zip ns (tail ns)]

-- Task 04
isImage :: [Int] -> [Int] -> Bool
isImage (a:as) (b:bs) = and [ai - bi == a - b | (ai, bi) <- zip as bs]

-- Task 05
chunksOf :: Eq a => Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
    | n > 0 = (take n xs) : (chunksOf n (drop n xs))
    | otherwise = []

-- Task 06
divisors :: Int -> [Int]
divisors n = [d | d <- [1..n-1], n `mod` d == 0]

-- Task 07
isTriangular :: [[Int]] -> Bool
isTriangular [] = True
isTriangular [[_]] = True
isTriangular xss = all (== 0) (tail (map head xss)) && isTriangular (tail (map tail xss))