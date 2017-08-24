--problem one 
myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs

--problem two
lastButOne :: [a] -> a
lastButOne [] = error "No but one for an empty list"
lastButOne [x] = x
lastButOne [x,y] = x
lastButOne (x:xs) = lastButOne xs


