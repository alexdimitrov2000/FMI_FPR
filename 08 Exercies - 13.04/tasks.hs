import Data.Char

main :: IO()
main = do
    print (normalizeMessage "Hello there")
    -- print (normalizeMessage "Hello 5 The3r3e") -- error
    print (encode ['A'..'Z'] 'Z' (-3))
    print (encode ['A'..'Z'] 'Z' 1)

-- Task 01
diff = (ord 'a' - ord 'A')

containsDigit (s:ms)
    | s >= '0' && s <= '9' = True
    | ms == [] = False
    | otherwise = containsDigit ms

normalizeMessage :: [Char] -> [Char]
normalizeMessage msg = 
    if containsDigit msg
        then error "digits not allowed"
    else
        map (\x -> if x >= 'a' && x <= 'z' then chr(ord x - diff) else x) letters
    where
        letters = [s | s <- msg, isLetter s]
            where
                isLetter c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

-- Task 02
-- a)
encode :: [Char] -> Char -> Int -> Char
encode alphabet ch offset = 
    if (offset >= 0)
        then offsetSymbol (findSymbol alphabet ch) offset
    else encode (reverse alphabet) ch (-offset)
        where
            findSymbol [] _ = error "unsupported character"
            findSymbol alphabet@(a:as) ch = 
                if a == ch then alphabet else findSymbol as ch
            
            offsetSymbol [] offset = offsetSymbol alphabet offset
            offsetSymbol (a:_) 0 = a
            offsetSymbol (_:as) offset = offsetSymbol as (offset - 1)

-- b)
encrypt :: [Char] -> Int -> String -> String
encrypt alphabet offset normalized = [encode alphabet ch offset | ch <- normalized]