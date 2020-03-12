main :: IO()
main = do
    print 2
    print (myGCD 12 18)
    print $ countDigits 1222237
    print $ countDigits2 1222237
    print (sumDigitsRec 12345)
    print (sumDigitsIter 12345)
    print (reverseNumber 123)
    print "Task 1 - Pow"
    print (pow 2 5)
    print "Task 2 - isPrime"
    print (isPrime 1)
    print (isPrime 2)
    print (isPrime 12)
    print (isPrime 11)
    print (isPrime 15)
    print "Task 3 - isAscending"
    print (isAscending 123)
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

{-
    Multi
    line
    comment
-}

myGCD :: Int -> Int -> Int
myGCD a b
    | a == b = a
    | a > b = myGCD (a - b) b
    | otherwise = myGCD a (b - a)

countDigits :: Int -> Int
countDigits n
    | n < 10 = 1
    | otherwise = 1 + countDigits (n `div` 10)

-- Other way
countDigits2 :: Int -> Int
countDigits2 n = 
    if n < 10
        then 1
    else 1 + countDigits(div n 10)

sumDigitsRec :: Int -> Int
sumDigitsRec n = 
    if n < 10
        then n
    else (n `mod` 10) + sumDigitsRec (n `div` 10)

sumDigitsIter :: Int -> Int
sumDigitsIter n = helper 0 n
    where 
        helper result 0 = result
        helper result k = helper (result + k `mod` 10) (k `div` 10)

reverseNumber :: Int -> Int
reverseNumber n = reverser 0 n
    where 
        reverser res 0 = res
        reverser res k = reverser (res * 10 + k `mod` 10) (k `div` 10)

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