module FirstList where

import qualified Data.Char as Char

-- ** Capitalize first words ** Problem 01
changeWord :: String -> String
changeWord [] = []
changeWord (x:xs) = Char.toUpper x : map Char.toLower xs

changeAll :: [String] -> [String]
changeAll [] = []
changeAll (x:xs) = changeWord x : changeAll xs

modernize :: String -> String
modernize [] = []
modernize (x:xs) = unwords (changeAll (words (x:xs)))

-- ** Extend date ** Problem 02
type Date = (Int, Int, Int)

printMonth :: Int -> String
printMonth m 
	| m == 1 = "January"
	| m == 2 = "February"
	| m == 3 = "March"
	| m == 4 = "April"
	| m == 5 = "May"
	| m == 6 = "June"
	| m == 7 = "July"
	| m == 8 = "August"
	| m == 9 = "September"
	| m == 10 = "October"
	| m == 11 = "November"
	| m == 12 = "December"
	| otherwise = error "Invalid month!"

printDay :: Int -> String
printDay d 
	| d == 1 || d == 21 || d == 31 = show d ++ "st "
	| d == 2 || d == 22 = show d ++ "nd "
	| d == 3 || d == 23 = show d ++ "rd "
	| d > 3 && d <= 30 = show d ++ "th "
	| otherwise = error "Invalid day!"

printYear :: Int -> String
printYear y = ", " ++ show y

showDate :: Date -> String
showDate (d, m, y) = printDay d ++ printMonth m ++ printYear y

-- ** Password strenght checker ** Problem 03
checkLength :: String -> Bool
checkLength a = length a >= 10

checkCap :: String -> Bool
checkCap a = or (map Char.isUpper a) && or (map  Char.isLower a)

checkNum :: String -> Bool
checkNum a = or (map Char.isDigit a)

strong :: String -> Bool
strong a = checkLength a && checkCap a && checkNum a

-- ** Binary tree ** Problem 04
data Tree = Tip | Node (Tree) Int (Tree)  

exampleTree = Node (Node (Node Tip 3 Tip) 8 (Node Tip 4 Tip)) 5 (Node Tip 6 Tip)

sumT :: Tree -> Int
sumT Tip = 0 
sumT (Node t1 a t2) = sumT t1 + a + sumT t2 
