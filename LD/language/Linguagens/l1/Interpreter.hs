module Interpreter where 

import Language

-- Nosso interpretador recebe uma expressao
-- e retorna um valor. 
eval :: Exp -> Value   
eval (IConst x) = IntValue x
eval (BConst x) = BooleanValue x

-- a avaliacao de expressoes booleanas / aritmeticas 
-- envolve a checagem de tipos.
eval e@(Add lhs rhs) = 
 case (baseType e) of
  (IntType) -> let 
                (IntValue l) = eval lhs
                (IntValue r) = eval rhs
               in IntValue (l + r)
  otherwise -> error $ "Wrong datatypes in " ++ (show e) 

eval e@(And lhs rhs) = 
 case (baseType e) of
  (BooleanType) -> let
                    (BooleanValue l) = eval lhs
                    (BooleanValue r) = eval rhs
                   in BooleanValue (l && r)
  otherwise -> error $ "Wrong datatypes in " ++ (show e) 

eval e@(Or lhs rhs) = 
 case (baseType e) of
  (BooleanType) -> let
                    (BooleanValue l) = eval lhs
                    (BooleanValue r) = eval rhs
                   in BooleanValue (l || r)
  otherwise -> error $ "Wrong datatypes in " ++ (show e) 



eval e@(Not exp) =
 case (baseType e) of
  (BooleanType) -> let
                    (BooleanValue v) = eval exp
                   in BooleanValue (not v)
  otherwise -> error $ "Wrong datatypes in " ++ (show e) 
                     
-- implementar as demais avaliacoes. 
-- Possivel reusar codigo?
