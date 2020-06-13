import Data.Char
import Data.List

main :: IO()
main = do
    print (ord 'a')
    print (chr 50)
    print (fromIntegral 2)
    print (ceiling (2.0 ^ 10))
    print (ceiling 2.56)
    print (floor 2.56)
    print (floor 2.0)
    print (2345678 `div` (ceiling (2.0 ^ 10)))
    print (normalize "Attack London tomorrow at ten a.m.")
    --print (normalize "Attack London tomorrow at 10 a.m.")
    print (encode ['A'..'Z'] 'A' 1)    -- = 'B'
    print (encode ['A'..'Z'] 'C' 2)    -- = 'E'
    print (encode ['A'..'Z'] 'Z' 3)    -- = 'C'
    print (encode ['A'..'Z'] 'A' (-1)) -- = 'Z'
    print (encode ['A'..'Z'] 'C' (-2)) -- = 'A'
    print (encode ['A'..'Z'] 'Z' (-3)) -- = 'W'
    --print (encode ['A'..'Z'] '@' 1)    -- = error “unsupported symbol: @”
    print (((encrypt ['A'..'Z'] 5) . normalize) "Attack London tomorrow at ten a.m.")
    print (((decrypt ['A'..'Z'] 5) . (encrypt ['A'..'Z'] 5) . normalize) "Attack London tomorrow at ten a.m.")
    print (substring "Haskell" "Haskell Curry")
    print (substring "asd" "asasd")
    print (polyencrypt ['A'..'Z'] 5 1 7 "ATTACKLONDONTOMORROWATTENAM")
    print (polydecrypt ['A'..'Z'] 5 1 7 "FYYFHPQUTJUTZUTVYYVDHBBMVIU")
    print (enigmaencrypt ['A'..'Z'] [(5,1,1),(7,2,10),(13,3,25)] "ATTACKLONDONTOMORROWATTENAM")

normalize :: String -> String
normalize "" = ""
normalize (c:cs)
    | '0' <= c && c <= '9' = error "digits not allowed"
    | 'A' <= c && c <= 'Z' = c:normalize cs
    | 'a' <= c && c <= 'z' = chr (ord c - ord 'a' + ord 'A'):normalize cs
    | otherwise            = normalize cs

encode :: [Char] -> Char -> Int -> Char
encode alphabet ch offset =
    if offset >= 0
    then offsetSymbol (findSymbol alphabet ch) offset
    else encode (reverse alphabet) ch (-offset)
    where
        findSymbol [] _ = error "unsupported symbol: @"
        findSymbol alphabet@(a:as) ch =
            if a == ch then alphabet else findSymbol as ch
        
        offsetSymbol []     offset = offsetSymbol alphabet offset
        offsetSymbol (a:_)  0      = a
        offsetSymbol (_:as) offset = offsetSymbol as (offset - 1)

encrypt :: [Char] -> Int -> String -> String
encrypt alphabet offset normalized = [encode alphabet ch offset | ch <- normalized]

decrypt :: [Char] -> Int -> String -> String
decrypt alphabet offset = encrypt alphabet (-offset)

crackall :: [Char] -> String -> [[Char]]
crackall alphabet encrypted = [decrypt alphabet o encrypted | o <- [0..length alphabet]]

substring :: String -> String -> Bool
substring "" _ = True
substring _ "" = False
substring str1@(s:ss) (t:ts) =
    if (s == t)
        then isPrefix ss ts || substring str1 ts
    else substring str1 ts
        where
            isPrefix (_:_) "" = False
            isPrefix "" _ = True
            isPrefix (s:ss) (p:ps) = 
                if (s == p)
                    then substring ss ps
                else False

crackcandidates :: [Char] -> [String] -> String -> [String]
crackcandidates alphabet commonword encrypted = [decrypted | decrypted <- crackall alphabet encrypted, any (\ word -> substring word decrypted) commonword]

polyencrypt :: [Char] -> Int -> Int -> Int -> String -> String
polyencrypt _ _ _ _ "" = ""
polyencrypt alphabet offset step blockSize msg = encrypt alphabet offset (take blockSize msg) ++ polyencrypt alphabet (offset + step) step blockSize (drop blockSize msg)

polydecrypt :: [Char] -> Int -> Int -> Int -> String -> String
polydecrypt _ _ _ _ "" = ""
polydecrypt alphabet offset step blockSize encrypted = polyencrypt alphabet (-offset) (-step) blockSize encrypted

enigmaencrypt :: [Char] -> [(Int, Int, Int)] -> String -> String
enigmaencrypt alphabet rotors msg = foldr (\ (offset, step, blockSize) temp -> polyencrypt alphabet offset step blockSize temp) msg rotors

enigmadecrypt :: [Char] -> [(Int, Int, Int)] -> String -> String
enigmadecrypt alphabet rotors msg = foldr (\ (offset, step, blockSize) temp -> polydecrypt alphabet offset step blockSize temp) msg rotors