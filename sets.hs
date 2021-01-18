
unionSet :: Eq a => Set a -> Set a -> Set a
unionSet (Set []) (Set ys) = (Set ys)
unionSet (Set (x:xs)) (Set ys) = insertSet x (unionSet (Set xs) (deleteSet x (Set ys)))

intersectSet :: Eq a => Set a -> Set a -> Set a
intersectSet (Set []) (Set ys) = (Set [])
intersectSet (Set (x:xs)) (Set ys) | inSet x (Set ys) = insertSet x (intersectSet (Set xs) (Set ys))
                                   | otherwise = intersectSet (Set xs) (Set ys)

setDiffSet :: Eq a => Set a -> Set a -> Set a
setDiffSet (Set []) (Set ys) = (Set [])
setDiffSet (Set (x:xs)) (Set ys) | inSet x (Set ys) = setDiffSet (Set xs) (Set ys)
                                 | otherwise = insertSet x (setDiffSet (Set xs) (Set ys))

cardinalSet :: Eq a => Set a -> Int
cardinalSet (Set []) = 0
cardinalSet (Set (x:xs)) = 1 + cardinalSet (Set xs)
