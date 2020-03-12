main :: IO()
main = do
    print "Task 1 - Pow"
    print (pow 2 5)
    print (pow 3 3)
    print (pow 5 3)
    print "Task 2 - isPrime"
    print (isPrime 1)
    print (isPrime 2)
    print (isPrime 12)
    print (isPrime 11)
    print (isPrime 15)
    print "Task 3 - isAscending"
    print (isAscending 123)
    print (isAscending 111)
    print (isAscending 321)
    print "Task 4 - countOccurences"
    print (countOccurences 5 12355)
    print (countOccurences 2 12355)
    print (countOccurences 8 12355)
    print (countOccurences 1 1)
    print (countOccurences 1 111112355)
    print "Task 5 - isPerfect"
    print (isPerfect 6)
    print (isPerfect 28)
    print (isPerfect 496)
    print (isPerfect 10)
    print (isPerfect 15)
    print "Task 6 - sumPrimeDivisors"
    print (sumPrimeDivisors 10)
    print (sumPrimeDivisors 60)

pow :: Int -> Int -> Int
pow _ 0 = 1
pow x n = x * pow x (n - 1)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = helper 2
    where 
        helper d
            | d == n = True
            | n `mod` d == 0 = False
            | otherwise = helper (d + 1)

isAscending :: Int -> Bool
isAscending n 
    | n < 10 = True
    | (n `div` 10) `mod` 10 > (n `mod` 10) = False
    | otherwise = isAscending (n `div` 10)

countOccurences :: Int -> Int -> Int
countOccurences x n = counter 0 n
    where
        counter cnt 0 = cnt
        counter cnt n
            | x == n `mod` 10 = counter (cnt + 1) (n `div` 10)
            | otherwise = counter cnt (n `div` 10)

isPerfect :: Int -> Bool
isPerfect n = (n == sumDivisors 1 2)
    where
        sumDivisors sum d
            | d == n = sum
            | n `mod` d /= 0 = sumDivisors sum (d + 1)
            | otherwise = sumDivisors (sum + d) (d + 1)

sumPrimeDivisors :: Int -> Int
sumPrimeDivisors n = sumDivisors 0 2
    where
        sumDivisors sum d
            | d == n = sum
            | n `mod` d == 0 && isPrime d = sumDivisors (sum + d) (d + 1)
            | otherwise = sumDivisors sum (d + 1)