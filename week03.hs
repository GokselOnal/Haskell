let naturals = [1..]
evens = [n|n <- naturals, even n]

evens2 = [2*n| <- naturals]

small_squares1 = [n^2|n <- [1..999]]
small_squares2 = [n^2|n <- naturals, n < 1000]

funny x | halts x x = funny x
        | otherwise = True

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float|Rectangle Point Point deriving (Show)


surface::Shape->Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs(x2 - x1))*(abs(y2 - y1))


nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (remove x xs)
 where
 remove y [] = []
 remove y (z:zs) | y == z = remove y zs
                 | otherwise = z : remove y zs

delete :: Eq a => a -> [a] -> [a]
delete x [] = []
delete x (y:ys) | x == y = ys
                | otherwise = y : delete x ys

elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys) | x == y = True
               | otherwise = elem' x ys 