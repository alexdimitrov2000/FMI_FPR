main :: IO()
main = do
    print ("Task 01 - Get length of list")
    print (listLength [1, 2, 3])
    print (listLength [])
    print (listLength [1, 2, 3, 4, 5])

    print ("Task 02 - Get sum of list elements")
    print (listSum [1, 2, 3])
    print (listSum [1, 2, 3, 4, 5])
    print (listSum [])

    print ("Task 03 - Check if list contains element")
    print (listContains 2 [1, 2, 3])
    print (listContains 2 [])
    print (listContains 3 [1, 2, 3])
    print (listContains 5 [1, 2, 3])

    print ("Task 04 - Prime numbers in interval")
    print (primeNumsInIntervalList 1 10)
    print (primeNumsInIntervalList 1 20)
    print (primeNumsInIntervalList 1 50)

    print ("Task 05 - Delete first occurrence of element in list")
    print (deleteFirstOcc 2 [1, 2, 3, 2, 4])
    print (deleteFirstOcc 5 [1, 2, 3, 2, 4])
    print (deleteFirstOcc 2 [])
    
    print ("Task 06 - Delete all occurrences of element in list")
    print (deleteAllOcc 2 [1, 2, 3, 2, 4])
    print (deleteAllOcc 5 [1, 2, 3, 2, 4])
    print (deleteAllOcc 2 [])
    print (deleteAllOcc 2 (replicate 5 2))

    print ("Task 07 - Increment all elements in a list")
    print (incrementAllBy [1, 2, 3] 1)
    print (incrementAllBy [1, 2, 3, 4, 5] 3)
    print (incrementAllBy [0] 1)
    print (incrementAllBy [] 1)

-- Task 01
listLength :: [Int] -> Int
listLength list = getLength list 0
    where
        getLength [] cnt = cnt
        getLength list cnt = getLength (tail list) (cnt + 1)

-- Task 02
listSum :: [Int] -> Int
listSum list =
    if list == []
        then 0
    else (head list) + listSum (tail list)

-- Task 03
listContains :: Int -> [Int] -> Bool
listContains x [] = False
listContains x list = helper (head list) (tail list)
    where
        helper curr [] = (x == curr)
        helper curr rest 
            | (curr == x) = True
            | otherwise = helper (head rest) (tail rest)

-- Task 04
primeNumsInIntervalList :: Int -> Int -> [Int]
isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = helper 2
    where 
        helper d
            | d == n = True
            | n `mod` d == 0 = False
            | otherwise = helper (d + 1)
primeNumsInIntervalList a b = [x | x <- [a..b], isPrime x]

-- Task 05
deleteFirstOcc :: Int -> [Int] -> [Int]
deleteFirstOcc _ [] = []
deleteFirstOcc x (curr:rest)
    | x == curr = rest
    | otherwise = curr : deleteFirstOcc x rest

-- Task 06
deleteAllOcc :: Int -> [Int] -> [Int]
deleteAllOcc _ [] = []
deleteAllOcc x (curr:rest)
    | (x == curr) = deleteAllOcc x rest
    | otherwise = (curr : deleteAllOcc x rest)

-- Task 07
incrementAllBy :: [Int] -> Int -> [Int]
incrementAllBy [] _ = []
incrementAllBy list n = helper [] list
    where
        helper res [] = res
        helper res (fst:rest) = helper (res ++ [fst + n]) rest