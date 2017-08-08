myLast :: [a] -> a
myLast []     = []
myLast [a]    = a
myLast xs     = myLast (tail xs)
