import Data.List
import Data.Char

main :: IO()
main = do
    print (graphContainsPoints [(1, 3), (2, 4), (3, 6)] (+ 2)) -- False
    print (graphContainsPoints [(1, 3), (2, 4), (3, 5)] (+ 2)) -- True
    print (expandString "abcd")
    print (expandString "2(abc)")
    print (expandString "2(3(a)bc)")
    -- print (expandString "2(3(a)2(bc))")

-- Task 01
graphContainsPoints :: [(Double, Double)] -> (Double -> Double) -> Bool
graphContainsPoints ps f = and [f x == y | (x, y) <- ps]

-- Task 02
-- expandString :: String -> String
expandString str = helper 0 "" str
    where
        helper cnt ts "" = duplicate cnt ts
        helper cnt ts (c:cs)
            | (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') = helper cnt (ts ++ [c]) cs
            | c >= '0' && c <= '9' =
                if cnt == 0
                    then (duplicate cnt ts) ++ helper (increaseCnt cnt c) ts cs
                else helper (increaseCnt cnt c) ts cs
            | c == '(' = duplicate cnt (expandString (take1 cs 0)) ++ expandString (drop1 cs 0)
            
        fixCount c = if c == 0 then 1 else c
        
        duplicate cnt ts = concat (replicate (fixCount cnt) ts)
        
        increaseCnt cnt c = (cnt * 10 + (ord c - ord '0'))
        
        take1 "" _ = ""
        take1 (c:cs) cnt
            | c == ')' = if cnt == 0 then "" else c : take1 cs (cnt - 1)
            | c == '(' = c : take1 cs (cnt + 1)
            | otherwise = c : take1 cs cnt
        
        drop1 "" _ = ""
        drop1 (c:cs) cnt
            | c == ')' = if cnt == 0 then "" else drop1 cs (cnt - 1)
            | c == '(' = drop1 cs (cnt + 1)
            | otherwise = drop1 cs cnt

-- expandString str = helper 1 "" str
--     where
--         helper cnt ts "" = concat (replicate cnt ts)
--         helper cnt ts (c:cs)
--             | (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') = helper cnt (ts ++ [c]) cs
--             | c >= '0' && c <= '9' = helper (cnt + (ord c - ord '1')) ts cs
--             | c == '(' = helper cnt "" cs
--             | c == ')' = concat (replicate cnt ts) ++ (helper 1 "" cs)