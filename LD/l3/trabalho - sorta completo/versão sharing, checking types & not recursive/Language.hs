module Language where 

data Type = IntType
			| BooleanType 
			| Undefined
			| FunType [Type] Type
 deriving (Show, Eq)

data Value = IntValue Int
           | BooleanValue Bool
		   | FunVal FormalArgs Env Exp
		   | ExpVal Exp Env --That is, a exprV is just a wrapper that holds an expression and the environment of its definition. 
   deriving(Show, Eq)

type BoxedBool = (Bool, Value)
type Id = String 

type FormalArgs = [(Id, Type)]
type Args = [Exp]
type Binding = (Id, Exp)

type Env = [Binding]
data FuncDecl = FuncDecl Id FormalArgs Exp

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
		 | Fun FormalArgs Env Exp 
		 | App Exp Args 
		 | IfTEls Exp Exp Exp
		 | Comp Exp Exp
 deriving(Show, Eq)

valueToConst :: Value -> Exp
valueToConst (IntValue a) = IConst a
valueToConst (BooleanValue b) = BConst b
valueToConst (FunVal fargs fenv exp) = Fun fargs fenv exp
 
checkArgTypes :: FormalArgs -> Args -> Env -> Bool
checkArgTypes [] [] _= True
checkArgTypes [] _ _= False
checkArgTypes _ [] _= False
checkArgTypes ((a,b):xs) (y:ys) env
	| b == (baseType y [] env) = True && checkArgTypes xs ys env
	| otherwise = False

findBoundExp :: Id -> Env -> [Exp]
findBoundExp ref env = [e | (r, e) <- env, r == ref] 

findArgType :: Id -> FormalArgs -> Type
findArgType _ [] = Undefined
findArgType ref ((id, typ):xs)
	| id == ref = typ
	| otherwise = findArgType ref xs


findFuncDecls :: Id -> [FuncDecl] -> [FuncDecl] 
findFuncDecls n fds = [f | f@(FuncDecl fn fargs exp) <- fds, n == fn] -- pega todas funções declaradas e retorna só as do nome (Id) que passei
 {-
findFuncDecls :: Id -> [FuncDecl] -> [FuncDecl] 
findFuncDecls n fds = [f | f@(FuncDecl fn fargs exp) <- fds, n == fn] -- pega todas funções declaradas e retorna só as do nome (Id) que passei
 -}
	
baseTypeCall :: Exp -> Env -> Type
baseTypeCall e env = baseType e [] env
	
baseType :: Exp -> FormalArgs -> Env -> Type
baseType (IConst v) _ _ = IntType
baseType (BConst v) _ _ = BooleanType
baseType (And lhs rhs) fargs env = sameTypes (lhs, rhs) BooleanType fargs env
baseType (Or lhs rhs) fargs env = sameTypes (lhs, rhs) BooleanType fargs env
baseType (Add lhs rhs) fargs env = sameTypes (lhs, rhs) IntType fargs env 
baseType (Sub lhs rhs) fargs env = sameTypes (lhs, rhs) IntType fargs env 
baseType (Mult lhs rhs) fargs env = sameTypes (lhs, rhs) IntType fargs env 
baseType (Div lhs rhs) fargs env = sameTypes (lhs, rhs) IntType fargs env 
baseType (Not e) fargs env = sameType e BooleanType fargs env

-- o tipo base de uma referencia a um 
-- id corresponde ao tipo base da expressao 
-- associada ao mesmo id no ambiente env. Caso
-- o identificador nao esteja associado a uma 
-- expressao, o tipo eh indefinido (erro).
baseType (RefId i) fargs env = 
	let exps = findBoundExp i env 
	in case exps of 
		[] -> findArgType i fargs
		(x:xs) -> baseType x fargs env


-- na nossa implementacao, o tipo base de uma aplicacao de funcao
-- consiste em verificar se a quantidade de 
-- argumentos passados na aplicacao da funcao eh 
-- igual a quantidade de argumentos formais usados 
-- na declaracao de funcao. Caso afirmativo, verificamos 
-- se a expressao do corpo da funcao eh bem tipada em um novo 
-- amibente que associa os argumentos formais aos argumentos 
-- passados na chamada da funcao.  
baseType (Fun fargs' fenv exp) fargs env = FunType (map snd fargs') (baseType exp (fargs++fargs') (fenv++env)) --note que está incompleto...(como adicionar os argumetos nos tipos?)

baseType (App (Fun fargs' fenv exp) args) fargs env = let 
										env' = (zip (map fst fargs') args)
										in baseType exp fargs env'

-- o tipo base de uma expressao let verifica 
-- inicialmente se o tipo base da expressao associada 
-- ao identificador eh Undefined. Caso afirmativo, 
-- o tipo da expressao let como um todo corresponde ao 
-- a Undefined. Caso contrario (o tipo da expressao assiciada 
-- ao identificador eh diferente de Undefined), o tipo base 
-- da expressao let eh o tipo base da expressao que corresponde 
-- ao corpo da expressao let.
baseType (Let _ exp1 exp2) fargs env = 
 case baseType exp1 fargs env of 
  Undefined -> Undefined
  otherwise -> baseType exp2 fargs env

baseType (IfTEls cond exp1 exp2) fargs env =
	case baseType exp1 fargs env of
	Undefined -> Undefined
	type1 | ((baseType exp2 fargs env) == type1) -> type1 
	_ -> Undefined
	
-- Uma funcao auxiliar usada para checar se 
-- dois operandos de uma expressao possuem o 
-- mesmo tipo, retornando esse tipo caso verdadeiro e 
-- Undefined caso contrario.
sameTypes :: (Exp, Exp) -> Type -> FormalArgs -> Env -> Type
sameTypes (lhs, rhs) t fargs env
 | (baseType lhs fargs env == t) && (baseType rhs fargs env == t) = t
 | otherwise = Undefined
 

-- Uma funcao auxiliar usada para checar se 
-- um operando possui um determinado tipo, 
-- retornando esse tipo caso verdadeiro e Undefined 
-- caso contrario.
sameType :: Exp -> Type -> FormalArgs -> Env -> Type
sameType e t fargs env
 | (baseType e fargs env == t) = t
 | otherwise = Undefined
 