module Parser where

type Parser a = String -> [(a, String)]

return' :: a -> Parser a
return' v = \inp -> [(v, inp)]
