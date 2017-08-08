module FirstList where

import qualified Data.Char as Char


upper :: String -> String
upper (x:xs) = Char.toUpper x : map Char.toLower xs

split :: [String] -> [String]
split [] = []
split (x:xs) = upper x : split xs

modernize :: String -> String
modernize (x:xs) = unwords (split (words(x:xs) ))

-- escape :: Char -> Bool
-- escpae x = if x == ' ' then True else False

-- data Month = January | February | March | April | May | June |
--              July | August | September | October | November | December
--     deriving (Enum)


type Date = (Int, Int, Int)

showDate :: Date -> String
showDate (x,y,z) = formatDay x : formatMonth y : show z

formatMonth :: Int -> String
formatMonth m
    | m == 1    = "January, "
    | m == 2    = "February, "
    | m == 3    = "March, "
    | m == 4    = "April, "
    | m == 5    = "May, "
    | m == 6    = "June, "
    | m == 7    = "July, "
    | m == 8    = "August, "
    | m == 9    = "September, "
    | m == 10   = "October, "
    | m == 11   = "November, "
    | m == 12   = "December, "

formatDay :: Int -> String
formatDay d
    | d `elem` [1..10]  = show d + "th "
    | d `elem` [11..21] = show d + "st "
    | otherwise         = show d








-- formatYear :: Int -> String
