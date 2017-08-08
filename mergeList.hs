zipTogether :: [a] -> [b] -> [(a,b)]
zipTogether [] [] = []
zipTogether [] ys = []
zipTogether xs [] = []
zipTogether xs ys = (head xs, head ys) : (zipTogether (tail xs) (tail ys))



zipTogether2 :: [a] -> [b] -> [(a,b)]
zipTogether2 [] [] = []
zipTogether2 [] ys = []
zipTogether2 xs [] = []
zipTogether2 (x:xs) (y:ys) = (x,y) : zipTogether2 xs ys


fac :: Int -> Int
fac n = product[1..n]

fib :: Int -> Int
fib n
  | n == 0  = 0
  | n == 1  = 1
  | n > 1   = n * fib(n-1)


-- facList :: [Int] -> [Int]
-- facList [] = []
-- facList xs = []
-- facList (x:xs) = fac x : (facList xs)

applyInList :: (Int -> Int) -> [Int] -> [Int]
applyInList f [] = []
applyInList f (x:xs) = (f x) : applyInList f xs
