elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs)
 | y == x    = True
 | otherwise = elem' y xs 


nub' :: (Eq a) => [a] -> [a]
nub' (x:xs) = (reverse (nubAux xs [x]))

nubAux :: (Eq a) => [a] -> [a] -> [a]
nubAux [] x = x
nubAux (x:xs) y = nubAux xs (if elem' x y then y else (x:y)) 

isAsc' :: [Int] -> Bool
isAsc' (x:xs)
 | xs == [] = True
 | x > head(xs) = False
 | otherwise = isAsc' xs

findSnd :: [(Int,Int)] -> Int -> [Int] ->[Int]
findSnd [] _ l = l
findSnd (x:xs) y l = if y == fst x then findSnd xs y ((snd x):l) else findSnd xs y l

fstExist :: [(Int, Int)] -> Int -> Bool
fstExist x y = elem' y (map fst x)


hasPath :: [(Int,Int)] -> Int -> Int -> Bool
hasPath x y z
 | fstExist x y = let k = findSnd x y [] in hasPath' x k y z []
 | otherwise = False


hasPath' :: [(Int,Int)] -> [Int] -> Int -> Int -> [Int] -> Bool
hasPath' x [] y z l = False


hasPath' x (k:ks) y z [] 
 | k == z = True
 | fstExist x k && hasPath' x (findSnd x k []) k z (k:[]) = True
 | otherwise = hasPath' x ks y z [] 


hasPath' x (k:ks) y z l 
 | k == z = True
 | elem' y (init l) = False
 | fstExist x k && hasPath' x (findSnd x k []) k z (k:l) = True
 | otherwise = hasPath' x ks y z l 



