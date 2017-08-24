module Strings where 

-- essa eh a solucao mais elegante que encontrei, 
-- pensei no caminho para a sala.
-- podem existir melhores, e testei 
-- pouco. Bom, acabei de achar um bug simples 
-- de resolver, mas discutimos na proxima aula.
-- essa estrategia nao usa acumuladores. 
split :: Char -> String -> [String]
split chr []  = []
split chr str = 
 let 
  first = takeWhile (/= chr) str
  remaining =  (dropWhile (/= chr) str)
 in case remaining of 
  []     -> [first]
  (x:xs) -> first : split chr xs

--- solucao com o uso de acumuladores. notem como 
--- aumentamos os argumentos da funcao split, na definicao
-- de split'' 
split' :: Char -> String -> [String]
split' chr [] = []
split' chr str = split'' chr [] "" str
 where 
   split'' :: Char -> [String] -> String -> String -> [String]
   split'' chr strings acm [] = acm +++ strings
   split'' chr strings acm (c:cs) 
    | chr == c && acm /= "" = split'' chr (acm +++ strings) "" cs
    | c == ' ' = split'' chr strings acm cs   
    | otherwise =  split'' chr strings (acm ++ [c]) cs   

-- funcao auxiliar que adiciona uma string (str) 
-- a uma lista de strings caso str nao seja uma 
-- string vazia ("") ou nao seja " ". 
(+++) :: String -> [String] -> [String]
(+++) " " strings = strings
(+++) ""  strings = strings 
(+++) str strings = strings ++ [str]  

 
