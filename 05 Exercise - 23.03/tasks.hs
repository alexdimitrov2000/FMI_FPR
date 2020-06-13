main :: IO()
main = do
    print ((\ x -> x * 3) 2)
    print (f1 (\ x -> x * 3) 3)
    print (f2 6)
    print ((f3 5) 2)
    print (f4 3)
    print (1 + 2)
    print ((1 +) 2)
    print ((+ 1) 2)
    print (map (+ 2) [1, 2, 3])
    print (map ((+ 2).(* 2)) [1, 2, 3])
    print (map ((* 2).(+ 2)) [1, 2, 3])
    
    -- Task 01
    print (myIdentity 20)
    print (myCompose (+ 2) (* 3) 4)
    print (myNegate isEven 10)
    print (myNegate isEven 11)

    -- Task 02
    print (difference (+ 3) 7 10)
    print (difference (* 2) 2 5)

    -- Task 03
    print (multiplyByTwo 10)

    -- Task 06
    print ((repeated (+ 2) 3) 5) -- 5 + 2 + 2 + 2 = 11 -> the function (+ 2) is repeated 3 times
    print ((repeated (\x -> x * x) 3) 2) -- 2 * 2 -> 4 * 4 -> 16 * 16 = 256 

f1 :: (Int -> Int) -> Int -> Int
f1 g x = g (2 * x)

f2 :: (Int -> Int)
f2 = \ x -> x + 1

f3 :: Int -> (Int -> Int)
f3 x = \ z -> 2 * x + z

f4 :: Int -> Int
f4 = f3 2

-- Task 01
-- a)
myIdentity :: a -> a
myIdentity x = x

-- b)
myCompose :: (a -> b) -> (c -> a) -> c -> b
myCompose f g = \ x -> f (g x)

-- c)
isEven :: Int -> Bool
isEven n = (n `mod` 2 == 0)

myNegate :: (a -> Bool) -> (a -> Bool)
myNegate f = \ x -> (not (f x))
-- myNegate f = (not . f)

-- d)
myCurry :: (a -> b -> c -> d) -> a -> (b -> c -> d)
myCurry f x = (f x)

-- Task 02
difference :: (Double -> Double) -> Double -> Double -> Double
difference f a b = (f b) - (f a)

-- Task 03
multiplyByTwo :: (Int -> Int)
multiplyByTwo = \ x -> 2 * x

-- Task 04
derive :: (Double -> Double) -> Double -> (Double -> Double) 
derive f eps = \ x -> (f (x + eps) - f x) / eps

-- Task 05
derive2 :: (Double -> Double) -> Double -> (Double -> Double) 
derive2 f eps = derive (derive f eps) eps

-- Task 05
-- deriveN :: (Double -> Double) -> Int -> Double -> (Double -> Double) 
-- deriveN f n eps = deriveN (deriveN f (n - 1) eps) eps

-- Task 06
repeated :: (a -> a) -> Int -> (a -> a)
repeated f 0 = \ x -> x
repeated f n = f . (repeated f (n - 1))
-- repeated f n = \ x -> (f . (repeated f (n - 1))) x

-- Task 07
newtonSqrt :: Double -> Double
newtonSqrt n = helper 10
    where
        helper cx
            | abs (cx * cx - n) < 1e-4 = cx
            | otherwise = helper (cx - (cx * cx - n) / (2 * cx))