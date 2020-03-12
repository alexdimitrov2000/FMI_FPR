main :: IO()
main = do
    print(5)
    print(v1)
    print ' '
    print(myMin 15 6)
    print (inside 3 1 5)
    print (calcSquareAverage 2 3)
    print (fibRec 5)
    print (fibIter 5)
    print (sumSquares 5 3)
    print (average 5 3)
    print (myFact 5)

v1 :: Int
v1 = 10

myMin :: Int -> Int -> Int
myMin a b = 
    if a < b
        then a
    else b

inside x a b = x >= a && x <= b

calcSquareAverage a b = (a^2 + b^2) / 2

fibRec n = 
    if (n <= 1)
        then 1
    else fibRec(n - 2) + fibRec(n - 1)

fibIter n = 
    helper n 1 1

helper n prev curr = 
    if n <= 1
        then curr
    else helper (n - 1) curr (prev + curr)

-- Group 3 exercises
sumSquares a b = square a + square b
    where square x = x ^ 2

average a b = (a + b) / 2

myFact n = 
    if n == 1
    then 1
    else n * myFact(n - 1)