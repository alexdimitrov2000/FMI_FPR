import Data.List

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
dominates f g (x:xs) = abs (f x) >= abs (g x) && dominates f g xs

-- Task 06
type Student = String
type Subject = String
type Grade = Double

type Record = (Student, Subject, Grade)

records :: [Record]
records = [("Pesho", "History", 4.20), ("Gosho", "History", 4), ("Pesho", "Maths", 6)]

getSubject :: Record -> Subject
getSubject (_, sub, _) = sub

hardestSubject :: [Record] -> Subject
hardestSubject rs = fst lowestAvgGrade
    where
        subjects = nub [getSubject r | r <- rs]
        grades = [(subj, [grade | (_, subjName, grade) <- rs, subjName == subj]) | subj <- subjects]
        average gs = sum gs / fromIntegral (length gs)
        avgGrades = [(subj, average gds) | (subj, gds) <- grades]
        lowest g1@(_, a1) g2@(_, a2) = if (a1 <= a2) then g1 else g2
        lowestAvgGrade = foldl1 lowest avgGrades