module Interpreter where 

import Language 

eval :: Exp -> Int
eval (IConst v) = v
eval (Add lhs rhs) = eval lhs + eval rhs
eval (Sub lhs rhs) = eval lhs - eval rhs