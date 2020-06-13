main :: IO()
main = do
    print ("Task 01")
    print (primesInRange 1 10)
    print (primesInRange 10 20)
    print (primesInRange 20 50)

    print ("Task 02")
    print (prodSumDiv [1, 2, 3, 4, 5, 10, 6, 11] 6)

    print ("Task 03")
    print (isSorted [1, 2, 3, 4, 5])
    print (isSorted [1, 2, 6, 3, 4, 5])
    print (isSorted [1, 1, 1, 1, 1])

    print ("Task 04")
    print (insert 1 [2, 3, 4, 5, 6])
    print (insert 7 [1, 2, 3, 4, 5, 6])
    print (insert 3 [1, 2, 3, 4, 5, 6])

    print ("Task 05")
    print (merge [] [1,2,3])
    print (merge [1,2] [])
    print (merge [1,2] [3,4])
    print (merge [1,2,7,8] [3,4])
    print (merge [1,1,1,1,5] [3,4,6,10])

    print ("Task 06")
    print (insertionSort [1,3,2])
    print (insertionSort [1,3,2,2,5])
    print (insertionSort [1,6,8,3,2])
    print (insertionSort1 [1,3,2])
    print (insertionSort1 [1,3,2,2,5])
    print (insertionSort1 [1,6,8,3,2])

-- Task 01
isPrime :: Integer -> Bool
isPrime n = ([1, n] == [d | d <- [1..n], n `mod` d == 0])

primesInRange :: Integer -> Integer -> [Integer]
primesInRange a b = [n | n <- [a..b], isPrime n]

-- Task 02
divisorsSum n = helper 1
    where 
        helper k
            | k == n = k
            | n `mod` k == 0 = k + helper (k + 1)
            | otherwise = helper (k + 1)

prodSumDiv :: [Integer] -> Integer -> Integer
prodSumDiv xs k = product [x | x <- xs, isNatural x, (divisorsSum x) `mod` k == 0]
    where
        isNatural n = (n >= 0 && n <= 9)

-- Task 03
isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (a:b:xs) = (a <= b && isSorted (b:xs))

-- isSorted (a:b:xs)
--     | a > b = False
--     | a <= b && xs == [] = True
--     | otherwise = isSorted (b:xs)

-- Task 04
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert n ls@(f:xs)
    | n <= f = n:ls
    | otherwise = f : insert n xs

-- Task 05
merge :: [Int] -> [Int] -> [Int]
merge ls [] = ls
merge [] ls = ls
merge fs@(x:xs) ss@(y:ys)
    | x <= y = x : merge xs ss
    | otherwise = y : merge fs ys

-- Task 06
insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

insertionSort1 :: [Int] -> [Int]
insertionSort1 xs = foldr insert [] xs