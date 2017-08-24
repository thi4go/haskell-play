{-Primeiro exercicio da lista de linguagens declarativas-}
{-Validacao de numeros de cartoes de credito -}

	{-Daniella A. dos Angelos-}
	{-110010434-}

module ValidacaoCartao where 

luhn :: Integer -> Bool
luhn card =  (((sumDigits . doubleEveryOther . separate) card) `mod` 10) == 0

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (x `div` 10) + (x `mod` 10) + sumDigits xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther s@(x:xs) = if (length s `mod` 2 == 0) 
						then x*2 : doubleEveryOther xs
						else x : doubleEveryOther xs

separate :: Integer -> [Integer]
separate 0 = []
separate x = separate (x `div` 10) ++ [x `mod`10]

tst1 = luhn 5594589764218858
tst2 = luhn 1234567898765432
