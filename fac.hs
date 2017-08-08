fac :: Int -> Int
fac n
  | n == 0  = 1
  | n == 1  = 1
  | n > 1   = n * fac (n-1)



fac2 :: Int -> Int
fac2 n = n *
  case n of
    0 -> 1
    1 -> 1
    _ -> fac2 (n-1)


fac3 :: Int -> Int
fac3 n = n *
  if(n > 1) then fac3 (n-1)
  else n


fac4 n = product[1..n]
