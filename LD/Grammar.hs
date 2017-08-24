module Grammar where

import Prelude hiding(True,False)

data Exp = True
         | False
         | Zero
         | Succ Exp
         | Pred Exp
         | IsZero Exp
         | ITE Exp Exp Exp
    deriving(Show)


eval :: Exp -> Exp
eval True   = True
eval False  = False
eval Zero   = Zero
eval (Pred Zero) = Zero

eval (IsZero Zero) = True
eval (IsZero exp1) = eval (IsZero eval(exp1))
eval (Succ exp1)        = Succ (eval exp1)
eval (Pred (Succ exp1)) = eval exp1

eval (ITE True exp2 exp3)  = eval exp2
eval (ITE False exp2 exp3) = eval exp3
eval (ITE exp1 exp2 exp3)  = eval (ITE (eval exp1) exp2 exp3)
