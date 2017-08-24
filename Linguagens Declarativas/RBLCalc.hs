module RBLCalc where

import Data.Bool


--SINTAXE ABSTRATA

type Id = String

data Exp = B Bool
         | N Integer
         | Add Exp Exp
         | Sub Exp Exp
         | And Exp Exp
         | Or Exp Exp
         | Not Exp
         | ITE Exp Exp Exp
         | Let Id Exp Exp
         | Ref Id
         | Lambda Id Exp
         | Appl Exp Exp


-- SEMANTICA OPERACIONAL (INTERPRETADOR)



eval :: Exp -> Exp -- o retorno dessa func poderia ser um int, uma func, um bool ... ou uma exp em forma normal

eval (B b)          = B b
eval (N n)          = N n
eval (And e1 e2)    = e1' && e2'
    where (B e1')   = eval e1
          (B e2')   = eval e2
eval (Not e1)       = B e1'
    where (B e1')   = eval e1

eval (Let x e1 e2)  = eval (Subst x e1 e2)

eval (ITE c e1 e2)
    | (eval c) == (B True)  = eval e1
    | (eval c) == (B False) = eval e2
    | otherwise             = error "type error"

eval (Lambda x e1)  = Lambda x e1 -- funções como valores. abstração 

eval (Appl e1 e2) = eval (Subst x e2 e)  -- aplicando e2 em e1. substituir as ocorrencias de x em e1 por e2
    where (Lambda x e) = eval e1




