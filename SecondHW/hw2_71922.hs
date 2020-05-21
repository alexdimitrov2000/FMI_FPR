main :: IO()
main = do
    print ("Task 01")
    print (generate 1 3)
    print (generate 0.1 5)

    print ("Task 02")
    print (listSquares 1 30)
    print (listSquares 250 300)

    print ("Task 03")
    print (splitPoints (1,1) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)])
    print (splitPoints (10,10) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)])

    print ("Task 04")
    print (getAverageBalance (as, ps) (\ (_,_,city) -> city == "Burgas"))
    print (getAverageBalance (as,ps) (\ (_,(n:_),_) -> n == 'P'))

-- Task 01
toNum :: Double -> Double -> Double
toNum pow denom
    | denom == 1 = 1.0
    | otherwise = (1 / (denom ** pow)) + toNum pow (denom - 1)
        
generate :: Double -> Int -> [Double]
generate p n = reverse (getList n)
    where
        getList maxDenom = 
            if (maxDenom == 1)
                then [1]
            else
                toNum p (fromIntegral maxDenom) : getList (maxDenom - 1)
        

-- Task 02
isPerfectSquare n = helper 1
    where
        helper k
            | k * k == n = True
            | k * k > n = False
            | otherwise = helper (k + 1)

listSquares :: Integer -> Integer -> [(Integer, Integer)]
listSquares a b = [(n, divisorsSquareSum n) | n <- [a..b], isPerfectSquare n]
    where
        divisorsSquareSum n = sum [d * d | d <- [1..n], n `mod` d == 0]

-- Task 03
type Point = (Double, Double)

splitPoints :: Point -> Double -> [Point] -> ([Point], [Point])
splitPoints _ _ [] = ([], [])
splitPoints center radius pts = ([ip | ip <- pts, isInCircle ip], [op | op <- pts, not (isInCircle op)])
    where
        isInCircle :: Point -> Bool
        isInCircle pnt = ((fst pnt - fst center) ** 2 + (snd pnt - snd center) ** 2) <= (radius ** 2)

-- Task 04
type Account = (Int, Int, Double)
type Person = (Int, String, String)

ps :: [Person]
ps = [(1, "Ivan", "Sofia"), (2, "Georgi", "Burgas"), (3, "Petar", "Plovdiv"), (4, "Petya", "Burgas")]
as :: [Account]
as = [(1, 1, 12.5), (2, 1, 123.2), (3, 2, 13.0), (4, 2, 50.2), (5, 2, 17.2), (6, 3, 18.3), (7, 4, 19.4)]

getAverage :: [Double] -> Double
getAverage bs = sum bs / fromIntegral (length bs)

getAverageBalance :: ([Account], [Person]) -> (Person -> Bool) -> Double
getAverageBalance db f = getAverage userBalances
    where
        searchedUserIds = map (\ (id,_,_) -> id) (filter f (snd db))
        userBalances = [balance | (_,userId,balance) <- (fst db), any (userId==) searchedUserIds] 