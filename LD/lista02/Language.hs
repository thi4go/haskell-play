{-Modulo que especifica a semantica da linguagem-}

	{-Daniella A. dos Angelos-}
	{-110010434-}

module Language where

data Exp = IConst Int
	| BConst Bool
	| And Exp Exp
	| Or Exp Exp
	| Not Exp
	| Add Exp Exp
	| Sub Exp Exp
	| Mult Exp Exp
	| Div Exp Exp
	| Let Id Exp Exp
	| RefId Id
	| App Id Args
	| If Exp Exp Exp
	deriving(Show, Eq)

data Type = IntType
	| BooleanType
	| Undefined
	deriving(Show, Eq)

data Value = IntValue Int
	| BooleanValue Bool
	deriving(Show, Eq)

data FuncDecl = FuncDecl Id FormalArgs Exp
	deriving(Show)

type Id = String
type FormalArgs = [(Id, Type)]
type Args = [Exp]
type Binding = (Id, Exp)
type Env = [Binding]

-- FUNCTION THAT RETURNS THE BASE TYPE OF AN EXPRESSION
baseType :: Exp -> Env -> [FuncDecl] -> Type
baseType (IConst i) _ _ = IntType
baseType (BConst b) _ _ = BooleanType
baseType (And lhs rhs) env decls = sameType (lhs, rhs) BooleanType env decls
baseType (Or lhs rhs) env decls = sameType (lhs, rhs) BooleanType env decls
baseType (Not exp) env decls =
	if baseType exp env decls == BooleanType
	then BooleanType
	else Undefined
baseType (Add lhs rhs) env decls = sameType (lhs, rhs) IntType env decls
baseType (Sub lhs rhs) env decls = sameType (lhs, rhs) IntType env decls
baseType (Mult lhs rhs) env decls = sameType (lhs, rhs) IntType env decls
baseType (Div lhs rhs) env decls = sameType (lhs, rhs) IntType env decls
baseType (Let id exp1 exp2) env decls =
	case (baseType exp1 env decls) of
		Undefined -> Undefined
		otherwise -> baseType exp2 ((id,exp1):env) decls
baseType (RefId id) env decls =
	case (findBoundExp id env) of
		[] -> Undefined
		(x:xs) -> baseType x env decls

baseType (App id args) env decls =
	case (findFuncDecl id decls) of
		[x@(FuncDecl _ fArgs exp)] -> 
			if checkNumArgs args x && checkTypes args fArgs env decls 
			then baseType (getFuncBody (findFuncDecl id decls))	
				(updateEnv (getFuncLink args (findFuncDecl id decls)) env) decls
			else Undefined
		otherwise -> Undefined

baseType (If cond exp1 exp2) env decls =
	case baseType cond env decls of
		BooleanType -> 
			if (baseType exp1 env decls) == (baseType exp2 env decls)
			then baseType exp1 env decls
			else Undefined
		otherwise -> Undefined

-- FUNCTION THAT RETURNS IF THE TYPE OF AN EXPRESSION IS VALID
typeCheck :: Exp -> Env -> [FuncDecl] -> Bool
typeCheck (IConst i) _ _ = True
typeCheck (BConst b) _ _ = True
typeCheck e@(And lhs rhs) env decls = (baseType e env decls) /= Undefined
typeCheck e@(Or lhs rhs) env decls = (baseType e env decls) /= Undefined
typeCheck e@(Not exp) env decls = (baseType e env decls) /= Undefined
typeCheck e@(Add lhs rhs) env decls = (baseType e env decls) /= Undefined
typeCheck e@(Sub lhs rhs) env decls = (baseType e env decls) /= Undefined
typeCheck e@(Mult lhs rhs) env decls = (baseType e env decls) /= Undefined
typeCheck e@(Div lhs rhs) env decls = (baseType e env decls) /= Undefined
typeCheck e@(Let id exp1 exp2) env decls = (baseType e env decls) /= Undefined
typeCheck e@(RefId id) env decls = (baseType e env decls) /= Undefined
typeCheck e@(App id args) env decls = (baseType e env decls) /= Undefined
typeCheck e@(If cond exp1 exp2) env decls = (baseType e env decls) /= Undefined

-- AUXILIAR FUNCTION THAT RETURN THE TYPE OF TWO EXPRESSIONS
-- IF THEIR TYPES ARE EQUAL TO THE EXPECTED TYPE
sameType :: (Exp, Exp) -> Type -> Env -> [FuncDecl] -> Type
sameType (lhs, rhs) t env decls =
	if (baseType lhs env decls == t) && (baseType rhs env decls == t)
	then t
	else Undefined

findBoundExp :: Id -> Env -> [Exp]
findBoundExp id env = [e | (i, e) <- env, id == i]

findFuncDecl :: Id -> [FuncDecl] -> [FuncDecl]
findFuncDecl id []  = []
findFuncDecl id (f@(FuncDecl i _ _):xs) =
	if (id == i) 
	then f:(findFuncDecl id xs)
	else findFuncDecl id xs

-- FUNCTION THAT CHECKS IF THE NUMBER OF ARGUMENTS
-- IN THE CALL OF A FUNCTION MATCHES THE NUMBER OF ARGUMENTS OF THAT FUNCTION
checkNumArgs :: Args -> FuncDecl -> Bool
checkNumArgs xs (FuncDecl _ ys _) = length xs == length ys

-- FUNCTION THAT CHECKS IF THE TYPES OF ARGUMENTS
-- IN THE CALL OF A FUNCTION MATCHES THE TYPES OF ARGUMENTS OF THAT FUNCTION
checkTypes :: Args -> FormalArgs -> Env -> [FuncDecl] -> Bool
checkTypes [] [] _ _ = True
checkTypes (x:xs) ((_, y):ys) env decls =
	((baseType x env decls) == y) && (checkTypes xs ys env decls)

-- RETURNS THE BODY EXPRESSION OF AN FUNCTION
getFuncBody :: [FuncDecl] -> Exp
getFuncBody [(FuncDecl _ _ exp)] = exp

-- RETURNS THE LIST OF ASSOSIATIONS IN THE APPLICATION OF FUNCTIONS
getFuncLink :: Args -> [FuncDecl] -> Env
getFuncLink [] [(FuncDecl _ [] _)] = []
getFuncLink (x:xs) [(FuncDecl i ((id,tipo):ys) exp )] =
	((id, x)):getFuncLink xs [(FuncDecl i ys exp)]

-- RETURNS THE ENV UPDATED TO DEAL WITH APPLICATIONS
updateEnv :: Env -> Env -> Env
updateEnv xs [] = xs
updateEnv [] ys = ys
updateEnv (x:xs) ys = 
	if (elem x ys)
	then updateEnv xs ys
	else updateEnv xs (x:ys)


