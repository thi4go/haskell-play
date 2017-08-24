{-Interpretador da mini linguagem funcional implementada -}
{-para a segunda lista de exercicios de Linguagens Declarativas-}

	{-Daniella A. dos Angelos-}
	{-110010434-}

module Interpreter where

import Language

-- FUNCTION THAT EVALUATES AN EXPRESSION
eval :: Exp -> Env -> [FuncDecl] -> Value
eval (IConst i) _ _ = IntValue i
eval (BConst b) _ _ = BooleanValue b
eval e@(And lhs rhs) env decls =
	case baseType e env decls of
		Undefined -> errorMessage
		BooleanType -> oValueOP (&&) (eval lhs env decls)
					 (eval rhs env decls)

eval e@(Or lhs rhs) env decls =
	case baseType e env decls of
		Undefined -> errorMessage
		BooleanType -> oValueOP (||) (eval lhs env decls)
					 (eval rhs env decls)

eval e@(Not exp) env decls =
	case baseType e env decls of
		Undefined -> errorMessage
		BooleanType -> notValue (eval exp env decls)

eval e@(Add lhs rhs) env decls =
	case baseType e env decls of
		Undefined -> errorMessage
		IntType -> iValueOP (+) (eval lhs env decls) 
					(eval rhs env decls)

eval e@(Sub lhs rhs) env decls =
	case baseType e env decls of
		Undefined -> errorMessage
		IntType -> iValueOP (-) (eval lhs env decls) 
					(eval rhs env decls)

eval e@(Mult lhs rhs) env decls =
	case baseType e env decls of
		Undefined -> errorMessage
		IntType -> iValueOP (*) (eval lhs env decls) 
					(eval rhs env decls)

eval e@(Div lhs rhs) env decls =
	case baseType e env decls of
		Undefined -> errorMessage
		IntType -> iValueOP quot (eval lhs env decls) 
					(eval rhs env decls)

eval e@(Let id exp1 exp2) env decls = eval exp2 ((id,exp1):env) decls

eval (RefId id) env decls =
	case (findBoundExp id env) of
    	[] -> error "Impossivel encontrar referencia"
        (x:xs) -> eval x env decls

eval e@(App id args) env decls =
	case baseType e env decls of
		Undefined -> errorMessage
		otherwise ->
			eval (getFuncBody (findFuncDecl id decls)) 
				(updateEnv (getFuncLink args (findFuncDecl id decls)) env) decls

eval e@(If cond exp1 exp2) env decls =
	case baseType e env decls of
		Undefined -> errorMessage
		otherwise -> 
			if (eval cond env decls) == BooleanValue True
			then eval exp1 env decls
			else eval exp2 env decls

-- FUNCTIONS THAT OPERATES IN VALUES
iValueOP :: (Int->Int->Int) -> Value -> Value -> Value
iValueOP f (IntValue x) (IntValue y) = IntValue (x `f` y)
oValueOP :: (Bool->Bool->Bool) -> Value -> Value -> Value
oValueOP f (BooleanValue x) (BooleanValue y) = BooleanValue (x `f` y)
notValue :: Value -> Value
notValue (BooleanValue x) = BooleanValue (not x)

-- FUNCTION THAT RETURNS AN ERROR MESSAGE
errorMessage :: Value
errorMessage = error "Impossivel validar expressao. Erro de tipo!"

