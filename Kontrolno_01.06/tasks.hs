main :: IO()
main = do
    print ("Task 01")
    print (checkSequence [2, 9, 15])
    print (checkSequence [11, 14, 20, 27, 31])
    print (checkSequence [11, 14, 28, 27, 31])
    print (checkSequence [11, 14, 14, 29, 31])
    
    print ("Task 02")
    print (removeNb 26)
    print (removeS 26)
    print (removeNb 100)
    print (removeS 100)
    print (removeNb 101)
    print (removeS 101)

    print ("Task 03")
    print (onDiag (5.5, 5.5))
    print (onDiag (0.5, 0))

-- Task 01
checkSequence :: [Int] -> Bool
checkSequence [f, s] = f < s && mod s f /= 0
checkSequence (f:s:xs) = f < s && mod s f /= 0 && checkSequence (s:xs)

-- Task 02
sumNumbers :: Int -> Int -> Int -> Int
sumNumbers n a b = sum [x | x <- [1..n], x /= a, x /= b]

removeNb :: Int -> [(Int, Int)]
removeNb n = [(a, b) | a <- [1..n], b <- [1..n], a * b == sumNumbers n a b]\

-- Task 03
type Point = (Double, Double)

line :: Point -> Point -> (Double -> Double)
line (x1, y1) (x2, y2) = \ x -> y1 + (x - x1) * (y2 - y1) / (x2 - x1)

liesOn :: (Double -> Double) -> (Point -> Bool)
liesOn f = \ (x, y) -> y == f x

diagonal :: (Double -> Double)
diagonal = line (0, 0) (1, 1)

onDiag :: Point -> Bool
onDiag = liesOn diagonal