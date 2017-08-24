module Interpreter where 

import Language

-- Nosso interpretador recebe uma expressao
-- e retorna um valor. 
eval :: Exp -> Env -> Value   
eval (IConst x) _ = IntValue x
eval (BConst x) _ = BooleanValue x

-- avalia a expressao exp2 em um ambiente 
-- env' atualizado, que adiciona a associacao 
-- (i, exp1) ao ambiente env. 
eval (Let i exp1 exp2) env = 
 let 
  env' = (i, exp1) : env 
 in eval exp2 env' 

-- avalia a expressao associada ao identificador 
-- ref (no ambiente env). Caso nao tenha uma associacao, 
-- uma excecao eh lancada.
eval (RefId ref) env = 
 let exps = findBoundExp ref env
 in case exps of 
     []  -> error $ " not in scope " ++ ref 
     (x:xs) -> eval x env

-- a avaliacao de expressoes booleanas / aritmeticas 
-- envolve a checagem de tipos. Mas isso foi delegado 
-- para as funcoes auxiliares evalBinBooleanExp e 
-- evalBinIntExp. 
eval e@(Add  lhs rhs) env  = evalBinIntExp e (lhs, rhs) (+) env
eval e@(Sub  lhs rhs) env  = evalBinIntExp e (lhs, rhs) (-) env
eval e@(Mult lhs rhs) env  = evalBinIntExp e (lhs, rhs) (*) env
eval e@(Div  lhs rhs) env  = evalBinIntExp e (lhs, rhs) div env
eval e@(And  lhs rhs) env  = evalBinBooleanExp e (lhs, rhs) (&&) env
eval e@(Or   lhs rhs) env  = evalBinBooleanExp e (lhs, rhs) (||) env
eval e@(Not   exp) env     = 
 case (baseType e env) of
  (BooleanType) -> let
                    (BooleanValue v) = eval exp env
                   in BooleanValue (not v)
  otherwise -> error $ "Wrong datatypes in " ++ (show e) 

-- Possivel simplificar ainda mais!!!! Trabalho para 
-- o aluno Andre ou Anayran.
evalBinBooleanExp :: Exp -> (Exp, Exp) -> (Bool -> Bool -> Bool) -> Env -> Value
evalBinBooleanExp e (lhs, rhs) op env = 
 case (baseType e env) of
  (BooleanType) -> let
                    (BooleanValue l) = eval lhs env 
                    (BooleanValue r) = eval rhs env 
                   in BooleanValue (l `op` r)
  otherwise -> error $ "Wrong datatypes in " ++ (show e) 


evalBinIntExp :: Exp -> (Exp, Exp) -> (Int -> Int -> Int) -> Env -> Value
evalBinIntExp e (lhs, rhs) op env = 
 case (baseType e env) of
  (IntType) -> let
                    (IntValue l) = eval lhs env 
                    (IntValue r) = eval rhs env 
                   in IntValue (l `op` r)
  otherwise -> error $ "Wrong datatypes in " ++ (show e) 
                     
-- implementar as demais avaliacoes. 
-- Possivel reusar codigo?
