module Interpreter where 

import Debug.Trace

import Language

updEnv :: Exp -> Env -> Exp
updEnv (Fun fargs fenv exp) env = Fun fargs (fenv++env) exp
updEnv (App n args) env = App n (map (\x -> updEnv x env) args)
updEnv (IfTEls cond exp1 exp2) env =  (IfTEls cond (updEnv exp1 env) (updEnv exp2 env))
updEnv (Comp exp1 exp2) env = Comp (updEnv exp1 env) (updEnv exp2 env)
updEnv (Add  lhs rhs) env = Add (updEnv lhs env) (updEnv rhs env)
updEnv (Sub  lhs rhs) env = Sub (updEnv lhs env) (updEnv rhs env)
updEnv (Mult lhs rhs) env = Mult (updEnv lhs env) (updEnv rhs env)
updEnv (Div  lhs rhs) env = Div (updEnv lhs env) (updEnv rhs env)
updEnv (And  lhs rhs) env = And (updEnv lhs env) (updEnv rhs env)
updEnv (Or   lhs rhs) env = Or (updEnv lhs env) (updEnv rhs env)
updEnv (Not   exp) env    = Not (updEnv exp env)
updEnv e _ = e

preprocess :: Exp -> Env -> Exp
preprocess (Let id exp1 exp2) env = App (Fun [(id, baseTypeCall exp1 env)] ((id,updEnv exp1 env):env) (preprocess exp2 ((id, updEnv exp1 env):env))) [updEnv exp1 env]
preprocess e env = e

--Não precisamos mais disso pq sumimos com expvalue :)

-- Nosso interpretador recebe uma expressao
-- e retorna um valor. 
strict :: Value -> Value
strict (ExpVal exp env )  = (trace "passou em strict indireto") value
	where value =  eval exp env
strict e = e
--}

eval :: Exp -> Env -> Value   
eval (IConst x) _= IntValue x
eval (BConst x) _ = BooleanValue x

-- avalia a expressao exp2 em um ambiente 
-- env' atualizado, que adiciona a associacao 
-- (i, exp1) ao ambiente env. 
eval l@(Let i exp1 exp2) env = eval (preprocess l env) env
{- A gente transforma let numa função agora, olha só que legal
	case exp1 of
	(Fun fargs exp envF) ->
		let env' = (i, Fun fargs exp (envF++env)) : env
		in eval exp2 env'
	_ ->
		let env' = (i, exp1) : env
		in eval exp2 env'
-}

-- avalia a expressao associada ao identificador 
-- ref (no ambiente env). Caso nao tenha uma associacao, 
-- uma excecao eh lancada.
eval (RefId ref) env = 
 let exps = findBoundExp ref env
 in case exps of 
     []  -> error $ " not in scope " ++ ref 
     (x:xs) -> eval x env

eval (Fun fargs fenv exp) env = FunVal fargs fenv exp

eval (App n args) env = 
	case  eval n env of
	FunVal fargs fenv exp->
		case checkArgTypes fargs args env of
			False -> error $ "Wrong variables types in function " ++ show(n)
			True ->	let 
					env' = zip (map fst fargs) (args) ++ fenv
					in eval exp env'
	otherwise -> error $ "You didn't give app a function you moron"


eval e@(IfTEls cond exp1 exp2) env =
	case baseTypeCall cond env of
	(BooleanType) -> 
		case baseTypeCall exp1 env of
		type1 | (baseTypeCall exp2 env) /= type1 -> error $ "Expresions of condition aren't the same"
		otherwise -> 
			case  eval cond env of
				BooleanValue (True) -> eval exp1 env
				BooleanValue (False) -> eval exp2 env
	otherwise -> error $ "Condition is not boolean"

eval (Comp exp1 exp2) env =
	case baseTypeCall exp1 env of
		Undefined -> error $ "First expression isn't well typed"
		type1 | (baseTypeCall exp2 env) /= type1 -> error $ "Expresions types conflicts"
		type1 | ((eval exp1 env) == (eval exp2 env)) -> BooleanValue True 
		_ -> BooleanValue False
		
-- a avaliacao de expressoes booleanas / aritmeticas 
-- envolve a checagem de tipos. Mas isso foi delegado 
-- para as funcoes auxiliares evalBinBooleanExp e 
-- evalBinIntExp. 
eval e@(Add  lhs rhs) env = evalBinIntExp e (lhs, rhs) (+) env
eval e@(Sub  lhs rhs) env = evalBinIntExp e (lhs, rhs) (-) env 
eval e@(Mult lhs rhs) env = evalBinIntExp e (lhs, rhs) (*) env
eval e@(Div  lhs rhs) env = evalBinIntExp e (lhs, rhs) div env
eval e@(And  lhs rhs) env = evalBinBooleanExp e (lhs, rhs) (&&) env
eval e@(Or   lhs rhs) env = evalBinBooleanExp e (lhs, rhs) (||) env
eval e@(Not   exp) env    = 
	case (baseTypeCall e env) of
		(BooleanType) -> let 
						(BooleanValue v) =  eval exp env
						in BooleanValue (not v)
		otherwise -> error $ "Wrong datatypes in " ++ (show e)

-- Possivel simplificar ainda mais!!!! Trabalho para 
-- o aluno Andre ou Anayran.
evalBinBooleanExp :: Exp -> (Exp, Exp) -> (Bool -> Bool -> Bool) -> Env -> Value
evalBinBooleanExp e (lhs, rhs) op env =
	case (baseTypeCall e env) of
		(BooleanType) -> let
						(BooleanValue l) =  eval lhs env
						(BooleanValue r) =  eval rhs env 
						in BooleanValue (l `op` r)
		otherwise -> error $ "Wrong datatypes in " ++ (show e)

evalBinIntExp :: Exp -> (Exp, Exp) -> (Int -> Int -> Int) -> Env -> Value
evalBinIntExp e (lhs, rhs) op env= 
	case (baseTypeCall e env) of
		(IntType) -> let 
					(IntValue l) =  eval lhs env
					(IntValue r) =  eval rhs env
					in IntValue (l `op` r) 
		otherwise -> error $ "Wrong datatypes in " ++ (show e)
 