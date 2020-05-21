main :: IO()
main = do
    print (filter (\ (_,_,city) -> city == "Burgas") ps)
    print (filter (\ (_,(n:_),_) -> n == 'P') ps)
    print (filter (\ (_,userId,_) -> userId == 2) as)
    print (map getBalance (filter (\ (_,userId,_) -> userId == 2) as))
    print (sum (map getBalance (filter (\ (_,userId,_) -> userId == 2) as)))
    print (any (11==) [0,1,2,3,1,1,5])

type Point = (Double, Double)

inner p r pt = sqrt ((fst pt - fst p) ^ 2 + (snd pt - snd p) ^ 2) <= r
innerPoints c r points = [x | x <- points, inner c r x]
outerPoints c r points = [x | x <- points, not(inner c r x)]

splitPoints :: Point -> Double -> [Point] -> ([Point], [Point])
splitPoints c r points = (innerPoints c r points, outerPoints c r points)

----------------
type Account = (Int, Int, Double)
type Person = (Int, String, String)

ps :: [Person]
ps = [(1, "Ivan", "Sofia"), (2, "Georgi", "Burgas"),
 (3, "Petar", "Plovdiv"), (4, "Petya", "Burgas")]
as :: [Account]
as = [(1, 1, 12.5), (2, 1, 123.2), (3, 2, 13.0), (4, 2, 50.2), (5, 2, 17.2), (6, 3, 18.3), (7, 4, 19.4)]

getBalance :: Account -> Double
getBalance (_, _, balance) = balance