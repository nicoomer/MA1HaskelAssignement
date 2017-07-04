sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []