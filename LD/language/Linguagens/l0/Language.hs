module Language where 

data Exp = IConst Int
         | Add Exp Exp
         | Sub Exp Exp

