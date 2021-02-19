lengthOfString :: String -> Integer
lengthOfString [] = 0
lengthOfString (x:xs) = 1 + lengthOfString xs

printLastElm :: String -> String
printLastElm (x:xs) | (lengthOfString xs) > 1 = printLastElm xs 
                    | otherwise = xs

removeLastElm :: String -> String
removeLastElm [] = ""
removeLastElm (x:xs) | (lengthOfString (x:xs)) > 1 = x : (removeLastElm xs)
                     | otherwise = xs

reverseString :: String -> String
reverseString [] = ""
reverseString [x] = [x]
reverseString a = (printLastElm a) ++ (reverseString(removeLastElm a))


