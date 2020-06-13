main :: IO()
main = do
    print (isInterestingNum 410)
    print (progressions [[1,2,3], [1, 3, 5], [2,4,5]])

-- Task 01
isInterestingNum :: Int -> Bool
isInterestingNum n = (n `mod` (digitsSum n) == 0)
    where
        digitsSum k
            | k == 0 = k
            | otherwise = k `mod` 10 + digitsSum (k `div` 10)

-- Task 02
consistSix::Int -> Bool
consistSix n
    |n == 0 = False
    |n `mod` 6 == 0 = True
    |otherwise = consistSix (n `div` 10)

-- Task 03
progressions :: [[Int]] -> [[Int]]
progressions lss = [es | es <- lss, isProgression es]
    where 
        isProgression [] = False
        isProgression [f] = True
        isProgression [f,s] = True
        isProgression (f:s:t:xs) = (f - s) == (s - t) && isProgression (s:t:xs)

-- Task 05
dominates :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominates f g [] = True
dominates f g (x:xs) = abs (f x) => abs (g x) && dominates f g xs

-- Task 06
type Student = String
type Subject = String
type Note = Double

type Record = (Student, Subject, Note)

-- hardestSubject :: [Record] -> Subject
-- hardestSubject [(st,su,n)] = getSubj
--     where
