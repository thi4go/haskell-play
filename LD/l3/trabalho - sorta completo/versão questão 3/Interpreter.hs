module Interpreter where 

import Language

-- Nosso interpretador recebe uma expressao
-- e retorna um valor. 
eval :: Exp -> Env -> [FuncDecl] -> Value   
eval (IConst x) _ _ = IntValue x
eval (BConst x) _ _ = BooleanValue x

-- avalia a expressao exp2 em um ambiente 
-- env' atualizado, que adiciona a associacao 
-- (i, exp1) ao ambiente env. 
eval (Let i exp1 exp2) env fds = 
 let env' = (i, exp1) : env
 in eval exp2 env' fds

-- avalia a expressao associada ao identificador 
-- ref (no ambiente env). Caso nao tenha uma associacao, 
-- uma excecao eh lancada.
eval (RefId ref) env fds = 
 let exps = findBoundExp ref env
 in case exps of 
     []  -> error $ " not in scope " ++ ref 
     (x:xs) -> eval x env fds

eval (App n args) env fds = 
 case findFuncDecls n fds of 
  []  -> error $ "Function " ++ n ++ " not declared"
  [(FuncDecl fn fargs exp)] -> 
	case checkArgTypes fargs args env fds of
		False -> error $ "Wrong variables types in " ++ n
		True -> let env' = (zip (map fst fargs) (map valueToConst (map (\x -> eval x env fds) args))) in eval exp env' fds
  (f1:f2:fs) -> error $ "Multiple declarations of " ++ n    
	
eval e@(IfTEls cond exp1 exp2) env fds =
	case eval cond env fds of
		BooleanValue (True) -> eval exp1 env fds
		BooleanValue (False) -> eval exp2 env fds
		otherwise -> error $ "Condition is not boolean"

eval (Comp exp1 exp2) env fds =
	case baseType exp1 env fds of
		Undefined -> error $ "First expression isn't well typed"
		type1 | (baseType exp2 env fds) /= type1 -> error $ "Expresions types conflicts"
		type1 | ((eval exp1 env fds) == (eval exp2 env fds)) -> BooleanValue True 
		_ -> BooleanValue False
-- a avaliacao de expressoes booleanas / aritmeticas 
-- envolve a checagem de tipos. Mas isso foi delegado 
-- para as funcoes auxiliares evalBinBooleanExp e 
-- evalBinIntExp. 
eval e@(Add  lhs rhs) env fds = evalBinIntExp e (lhs, rhs) (+) env fds
eval e@(Sub  lhs rhs) env fds = evalBinIntExp e (lhs, rhs) (-) env fds 
eval e@(Mult lhs rhs) env fds = evalBinIntExp e (lhs, rhs) (*) env fds
eval e@(Div  lhs rhs) env fds = evalBinIntExp e (lhs, rhs) div env fds
eval e@(And  lhs rhs) env fds = evalBinBooleanExp e (lhs, rhs) (&&) env fds
eval e@(Or   lhs rhs) env fds = evalBinBooleanExp e (lhs, rhs) (||) env fds
eval e@(Not   exp) env fds    = let (BooleanValue v) = eval exp env fds
								in BooleanValue (not v)

-- Possivel simplificar ainda mais!!!! Trabalho para 
-- o aluno Andre ou Anayran.
evalBinBooleanExp :: Exp -> (Exp, Exp) -> (Bool -> Bool -> Bool) -> Env -> [FuncDecl] -> Value
evalBinBooleanExp e (lhs, rhs) op env fds =
	let 
	  (BooleanValue l) = eval lhs env fds 
	  (BooleanValue r) = eval rhs env fds 
	in BooleanValue (l `op` r)

evalBinIntExp :: Exp -> (Exp, Exp) -> (Int -> Int -> Int) -> Env -> [FuncDecl] -> Value
evalBinIntExp e (lhs, rhs) op env fds = 
	let 
	  (IntValue l) = eval lhs env fds 
	  (IntValue r) = eval rhs env fds
	in IntValue (l `op` r)
 