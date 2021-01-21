count :: Char -> String -> Int
count c [] = 0
count c (x:xs) | x == c = 1 + count c xs
               | otherwise = count c xs
 
eleman :: String -> Int
eleman [] = 0
eleman (x:xs) = 1 + eleman xs 


takeOdds :: [Integer] -> [Integer]
takeOdds [] = []
takeOdds (x:xs) | (rem x 2 == 0) = takeOdds xs
                | otherwise = x : (takeOdds xs)


printN :: Char -> Int -> String
printN c n | n <= 0 = ""
           | otherwise = c : (printN c (n-1))

incPrintN :: String -> Int -> String
incPrintN [] n = []
incPrintN (x:xs) n = (printN x n) ++ (incPrintN xs (n+1))


blowup :: String -> String
blowup xs = incPrintN xs 1 

wholePart :: String -> String
wholePart (x:xs) | x == '.' = ""
                 | otherwise = x : (wholePart xs)

fracPart :: String -> String
fracPart (x:xs) | x == '.' = xs
                | otherwise = fracPart xs


lengths :: [[a]] -> [Int]
lengths [[]] = []
lengths arr = map length arr

sumLengths :: [[Int]] -> Int
sumLengths arr =  sum (lengths arr)
