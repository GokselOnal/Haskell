mnmInt::[Int] -> Int
mnmInt [] = error "empty list"
mnmInt[x] = x
mnmInt(x:xs) = min x (mnmInt xs)

min' :: Int -> Int -> Int
min' x y | x <= y = x
	 | otherwise = y

max' :: Int -> Int -> Int
max' a b | a >= b = a
	 | otherwise = b

removeFst :: [Int]->Int->[Int]
removeFst [] m = []
removeFst (x:xs) m | m == x= xs
		   | otherwise = (x:removeFst xs m)

srtInts :: [Int]->[Int]
srtInts [] = []
srtInts x = (mnmInt x) : (srtInts(removeFst x (mnmInt x)))

average :: [Int]->Rational
average [] = error "empty list"
average x = toRational(sum x) / toRational(length x)

checkC :: Char -> String -> Bool
checkC c [] = False
checkC c (x:xs) | c == x = True
		| otherwise = checkC c xs

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

maxL :: [Int] -> Int
maxL [] = 0
maxL (x:xs) = max x (maxL xs)

maxElement :: [[Int]] -> Int
maxElement a = maxL (map maxL a)

a = 3
b = 4
f :: Integer -> Integer -> Integer
f x y = x^2 + y^2

