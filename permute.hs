permute' :: [a] -> [[a]]
permute' [] = [[]]
permute' (x:xs) = insertAll x (permute' xs)

insertAll :: a -> [[a]] -> [[a]]
insertAll y [x] = [insertK k y x|k <- [0..(length x)]]
insertAll y (x:xs) = (insertAll  y [x]) ++ (insertAll y xs)

insertK :: Int -> a -> [a] -> [a]
insertK k y x = (take k x) ++ (y :(drop k x))

