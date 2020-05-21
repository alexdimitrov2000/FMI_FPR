main :: IO()
main = do
    print ("HW - Task 01 - findSum")
    print (findSum 5 3 5)
    print (findSum 0 2 10)
    print (findSum 0 1 4)

    print ("HW - Task 02 - isSquare")
    print (isSquare 1)
    print (isSquare 2)
    print (isSquare 4)
    print (isSquare 15)
    print (isSquare 16)
    print (isSquare 256)
    print (isSquare 2495)
    print (isSquare 2500)

    print ("HW - Task 03 - isSpecial")
    print (isSpecial 131 2)
    print (isSpecial 132 2)
    print (isSpecial 17197 2)
    print (isSpecial 12234 3)
    print (isSpecial 10113 3)
    print (isSpecial 353 2)
    print (isSpecial 11317 2)
    print (isSpecial 11317 3)
    print (isSpecial 12345 4)

-- Task 1
{- 
Функция, която приема 3 Int параметри и връща Int

За пресмятането на сбора на последните 3 числа от редицата се ползва помощна функция calculateSumOfLastThree
за целта, не започвам от първото число, а от последното, като по този начин не хабя ресурс да пресмятам всичките числа от редицата
calculateSumOfLastThree има за първи параметър текущата степен, която ще използваме при пресмятането на 2^k * b
вторият параметър на помощната функция го използваме, за да пазим резултата на елемента от редицата
третият параметър е брояч, който ни помага да пресметнем само последните 3 числа, а не повече, съответно програмата връща крайния резултат, когато numsCnt е станало 3 и е пресметнато и последното трето число
Тъй като numsCnt започва от 0, можем да го използваме, за да намаляваме впоследствие началната степен n
На всяка итерация сравняваме текущата степен с началната и преценяваме дали да продължим да пресмятаме сегашния елемент или трябва да продъжим със следващия (в моя случай е предния елемент, а не следващия)
-}
findSum :: Int -> Int -> Int -> Int
findSum a b n = calculateSumOfLastThree 0 0 0
    where 
        calculateSumOfLastThree k res numsCnt
            | numsCnt == 3 = res
            | (k < (n - numsCnt)) = calculateSumOfLastThree (k + 1) (res + b * (2 ^ k)) numsCnt
            | (k == (n - numsCnt)) = (a + res) + calculateSumOfLastThree 0 0 (numsCnt + 1)

-- Task 2
{-
Функция, която приема единствен параметър от тип Int и връща като резултат Bool

1. Има два конкретни случая, когато параметърът е 1 или 2, и можем да върнем директно резултат
2. Когато получим като параметър число, по-голямо от 2, използваме помощна функция helper за връщането на крайния резултат.
helper получава параметър с начална стойност 2
2.1 На всяка итерация проверяваме дали квадратът на параметъра на helper cnt е равен на числото n, получено от условието.
Ако е равно, то числото n е квадрат на някое друго число и връщаме True като краен резултат
2.2 Ако cnt ^ 2 е по-голямо от n, това означава, че няма число cnt + 1, което на квадрат да е равно на n и затова връщаме False
2.3 Във всеки друг случай връщаме пак helper с увеличен cnt и пак преминаваме през стъпките по-горе, докато не получим краен Bool резултат 
-}
isSquare :: Int -> Bool
isSquare 1 = True
isSquare 2 = False
isSquare n = helper 2
    where 
        helper cnt
            | cnt ^ 2 == n = True
            | cnt ^ 2 > n = False
            | otherwise = helper (cnt + 1)

-- Task 3
{-
Функция-предикат, която приема две цели числа и връща Bool

1. Ако първият параметър n е число с по-малко от k на брой цифри, то връщаме True
2. Изразът n `mod` (10 ^ k) връща числото, съставено от последните k цифри на n, след което проверяваме дали е просто
2.1 ако не е просто число, то n не е специално и връщаме False като краен резултат
2.2 ако е просто, то махаме последната цифра на n и пак връщаме същата функция isSpecial
-}
isSpecial :: Integer -> Int -> Bool
isSpecial n k
    | n < 10 ^ (k - 1) = True
    | isPrime (n `mod` (10 ^ k)) == False = False
    | isPrime (n `mod` (10 ^ k)) == True = isSpecial (n `div` 10) k
    where
        isPrime n = ([1, n] == [d | d <- [1..n], n `mod` d == 0])
