-- Vectors and Lists Exercise
main :: IO()
main = do
    print ("Vectors")
    print (pesho) -- ("Pesho","Petrov",20)
    print (point) -- (2,3)
    print (fst point) -- 2
    print (snd point) -- 3
    print (sumVectors (1, 2) (2, 3)) -- (3, 5)
    print (sumVectors (1, 2) point) -- same as the above row

    print ("Lists")
    print ([2..5]) -- [2,3,4,5]
    print (['a'..'d']) -- "abcd"
    print (oddNums) -- [1,3,5,7,9,11] -> numbers from 1 to 11 with step 2 (all odd numbers from 1 to 11)
    print ([1,3..10]) -- [1,3,5,7,9] -> the step is 2 => we print only odd numbers, but the end num of the list is 10 (even) so that we print the odd numbers before it
    print ([2 * n | n <- oddNums]) -- [2,6,10,14,18,22] => print the doubled numbers from the oddNums list
    print ([2 * n | n <- oddNums, n > 5]) -- [2,6,10,14,18,22] => print the doubled numbers from the oddNums list where n is more than 5
    print (addPairs [(1, 2), (3, 4), (5, 6)]) -- [3, 7, 11]
    print (addPairsWithCondition [(1, 2), (4, 3), (5, 6), (8, 7)]) -- [7, 15] => summing only the pairs where the condition is fulfilled (second and fourth pairs)
    print (areAllEven [1,2,3,4,5]) -- false
    print (areAllEven [0,2,4,6]) -- true

    print ("List operations")
    print (1 : [2, 3, 4]) -- [1, 2, 3, 4] => operator ':' adds an element to the front of the list
    print ([1, 2, 3] ++ [3, 4, 5]) -- [1, 2, 3, 3, 4, 5] => operator '++' concatenates lists
    print ([1, 2, 3] !! 2) -- 3 => operator ([a] !! n) returns element at position n in the list [a], starting from 0
    print (concat [[1], [], [1, 2, 3]]) -- [1, 1, 2, 3] => 'concat' concatenates a list of lists into a single list
    print (length [1, 2, 3, 4, 10]) -- 5 => 'length' returns the number of elements in the list
    print (head listFromOneToFive) -- 1 => 'head' returns the first element from the list
    print (last listFromOneToFive) -- 5 => 'last' returns the last element from the list
    print (tail listFromOneToFive) -- [2, 3, 4, 5] => 'tail' returns the all elements except for the first one
    print (init listFromOneToFive) -- [1, 2, 3, 4] => 'last' returns the all elemenets except for the last one
    print (replicate 5 1) -- [1, 1, 1, 1, 1] => 'replicate n k' returns a list of n repetitions of the element k
    print (take 3 listFromOneToFive) -- [1, 2, 3] => 'take n [a]' return the first n elements from the list [a]
    print (drop 2 listFromOneToFive) -- [3, 4, 5] => 'drop n [a]' return the list [a] without the first n elements
    print (splitAt 2 listFromOneToFive) -- ([1, 2], [3, 4, 5]) => 'splitAt n [a]' splits the list at index n into two lists, which are returned as a pair
    print (reverse listFromOneToFive) -- [5, 4, 3, 2, 1] => 'reverse' returns the reversed list
    print (zip [1, 2] [3, 4, 5]) -- [(1, 3), (2, 4)] => 'zip' returns a list of pairs. the number of pairs is the length of the smaller list. the rest of the nums are ignored
    print (unzip [(1, 3), (2, 4), (7, 8)]) -- ([1, 2, 7], [3, 4, 8]) => 'unzip' converts a list of pairs into a pair of lists
    print (and [True, True, False]) -- False => (Conjunction) -> 'and' checks if all of the elements in a [Bool] list are True
    print (and [True, True, True]) -- True
    print (and (replicate 5 True)) -- True => 'replicate' creates a list of 5 True elements -> 'and' checks if all of them are True
    print (or [True, False, False]) -- True => (Disjunction) -> 'or' checks if the [Bool] list contains at least one True
    print (or (replicate 3 False)) -- False
    print (sum listFromOneToFive) -- 15 => 'sum' returns the sum of all elements
    print (sum [1.5, 2.5, 3, 4.3]) -- 11.3
    print (product listFromOneToFive) -- 120 => 'product' returns the product of all elements
    print (product (init listFromOneToFive)) -- 24 => product of all elements except for the last one

type Person = (String, String, Int) -- vector, representing the person's first name, last name and age
pesho :: Person
pesho = ("Pesho", "Petrov", 20)

type Point = (Int, Int)
point :: Point
point = (2, 3)

sumVectors :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumVectors v1 v2 = ((fst v1 + fst v2), (snd v1 + snd v2))

oddNums = [1,3..11] -- from 1 to 11

addPairs :: [(Int, Int)] -> [Int]
addPairs pairList = [m + n | (m, n) <- pairList]

addPairsWithCondition :: [(Int, Int)] -> [Int]
addPairsWithCondition pairList = [m + n | (m, n) <- pairList, m > n]

areAllEven list = (list == [x | x <- list, isEven x])
    where isEven :: Int -> Bool
          isEven n = (n `mod` 2 == 0)

listFromOneToFive :: [Int]
listFromOneToFive = [1, 2, 3, 4, 5]