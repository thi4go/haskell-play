module Language where 

data Type = IntType | BooleanType | Undefined
 deriving (Show, Eq)

data Value = IntValue Int
           | BooleanValue Bool
   deriving(Show, Eq)

type Id = String 

-- A linguagem basicamente consiste em 
-- Expressoes. Na versao 0, apenas expressoes 
-- envolvendo inteiros e valores booleanos.
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
 deriving(Show)

type Binding = (Id, Exp)

type Env = [Binding]

findBoundExp :: Id -> Env -> [Exp]
findBoundExp ref env = [e | (r, e) <- env, r == ref] 
 
baseType :: Exp -> Env -> Type
baseType (IConst v) _ = IntType
baseType (BConst v) _ = BooleanType
baseType (And lhs rhs) env =  sameTypes (lhs, rhs) BooleanType env
baseType (Or lhs rhs) env =  sameTypes (lhs, rhs) BooleanType env
baseType (Add lhs rhs) env = sameTypes (lhs, rhs) IntType env
baseType (Sub lhs rhs) env = sameTypes (lhs, rhs) IntType env
baseType (Mult lhs rhs) env = sameTypes (lhs, rhs) IntType env
baseType (Div lhs rhs) env = sameTypes (lhs, rhs) IntType env
baseType (Not e) env = sameType e BooleanType env


-- o tipo base de uma referencia a um 
-- id corresponde ao tipo base da expressao 
-- associada ao mesmo id no ambiente env. Caso
-- o identificador nao esteja associado a uma 
-- expressao, o tipo eh indefinido (erro).
baseType (RefId i) env = 
 let exps = findBoundExp i env 
 in case exps of 
     [] -> error $ "Not in scope " ++ i
     (x:xs) -> baseType x env

-- o tipo base de uma expressao let verifica 
-- inicialmente se o tipo base da expressao associada 
-- ao identificador eh Undefined. Caso afirmativo, 
-- o tipo da expressao let como um todo corresponde ao 
-- a Undefined. Caso contrario (o tipo da expressao assiciada 
-- ao identificador eh diferente de Undefined), o tipo base 
-- da expressao let eh o tipo base da expressao que corresponde 
-- ao corpo da expressao let.
baseType (Let _ exp1 exp2) env = 
 case baseType exp1 env of 
  Undefined -> Undefined
  otherwise -> baseType exp2 env

-- Uma funcao auxiliar usada para checar se 
-- dois operandos de uma expressao possuem o 
-- mesmo tipo, retornando esse tipo caso verdadeiro e 
-- Undefined caso contrario.
sameTypes :: (Exp, Exp) -> Type -> Env -> Type
sameTypes (lhs, rhs) t env 
 | (baseType lhs env == t) && (baseType rhs env == t) = t
 | otherwise = Undefined
 

-- Uma funcao auxiliar usada para checar se 
-- um operando possui um determinado tipo, 
-- retornando esse tipo caso verdadeiro e Undefined 
-- caso contrario.
sameType :: Exp -> Type -> Env -> Type
sameType e t env
 | (baseType e env == t) = t
 | otherwise = Undefined