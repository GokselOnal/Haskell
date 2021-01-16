delete :: Eq a => a -> [a] -> [a]
delete x [] = []
delete x (y:ys) | x == y = ys
                | otherwise = y:(delete x ys)

elem' :: Eq a => a -> [a] -> Bool
elem' c [] = False
elem' c (x:xs) | c == x = True
               | otherwise = elem' c xs

union :: Eq a => [a] -> [a] -> [a]
union [] ys = ys
union (x:xs) ys = x : (union xs (delete x ys))

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] ys = []
intersect (x:xs) ys | elem' x ys = x:(intersect xs ys)
                    | otherwise = intersect xs ys

setDiff :: Eq a => [a] -> [a] -> [a]
setDiff [] ys = []
setDiff (x:xs) ys | elem' x ys = setDiff xs ys
                  | otherwise =  x:(setDiff xs ys)

crossProList :: Eq a => [a] -> [a] -> [(a,a)]
crossProList xs ys = ((x,y) | x <- xs, y <- ys)