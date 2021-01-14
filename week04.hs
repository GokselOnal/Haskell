
union :: Eq a => [a] -> [a] -> [a]
union [] ys = ys
union (x:xs) ys = x : union xs (delete x ys)

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] s = []
intersect (x:xs) s | elem x s = x : intersect xs s
                   | otherwise = intersect xs s

addElem :: a -> [[a]] -> [[a]]
addElem x y = map (x:) y

powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList(x:xs)=(powerList xs)++(addElem x (powerList xs))

instance Eq a => Eq (Set a) where
 set1 == set2 = subSet set1 set2 && subSet set2 set1


emptySet :: Set a
emptySet = Set []

isEmpty :: Set a -> Bool
isEmpty (Set []) = True
isEmpty _ = False

inSet :: Eq a => a -> Set a -> Bool
inSet x (Set s) = elem x s

subSet :: (Eq a) => Set a -> Set a -> Bool
subSet (Set []) _ = True
subSet (Set (x:xs)) set = (inSet x set) && subSet(Set xs) set

insertSet :: (Eq a) => a -> Set a -> Set a
insertSet x (Set ys) | inSet x (Set ys) = Set ys
                     | otherwise = Set (x:ys)

deleteSet :: Eq a => a -> Set a -> Set a
deleteSet x (Set xs) = Set (delete x xs)

list2set :: Eq a => [a] -> Set a
list2set [] = Set []
list2set (x:xs) = insertSet x (list2set xs)

powerSet :: Eq a => Set a -> Set (Set a)
powerSet (Set xs) = Set (map (\xs -> (Set xs)) (powerList xs))

powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x:xs) = (powerList xs) ++ (map (x:(powerList xs))

takeSet :: Eq a => Int -> Set a -> Set a
takeSet n (Set xs) = Set(take n xs)

unionSet :: Eq a => Set a -> Set a -> Set a
unionSet (Set []) set2 = set2
unionSet (Set (x:xs)) set2 = insertSet x (unionSet (Set xs) set2)
