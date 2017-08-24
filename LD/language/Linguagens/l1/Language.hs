module Language where 

data Type = IntType | BooleanType | Undefined
 deriving (Show, Eq)

data Value = IntValue Int
           | BooleanValue Bool
   deriving(Show, Eq)

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
 deriving(Show)

baseType :: Exp -> Type
baseType (IConst v) = IntType
baseType (BConst v) = BooleanType
baseType (And lhs rhs) =  (lhs, rhs) `are` BooleanType
baseType (Or lhs rhs)  =  (lhs, rhs) `are` BooleanType
baseType (Add lhs rhs) =  (lhs, rhs) `are` IntType
baseType (Sub lhs rhs) =  (lhs, rhs) `are` IntType
baseType (Mult lhs rhs) =  (lhs, rhs) `are` IntType
baseType (Div lhs rhs) =  (lhs, rhs) `are` IntType
baseType (Not e) = e `is` BooleanType

-- Uma funcao auxiliar usada para checar se 
-- dois operandos de uma expressao possuem o 
-- mesmo tipo, retornando esse tipo caso verdadeiro e 
-- Undefined caso contrario.
are :: (Exp, Exp) -> Type -> Type
are (lhs, rhs) t 
 | (baseType lhs == t) && (baseType rhs == t) = t
 | otherwise = Undefined
 
-- Uma funcao auxiliar usada para checar se 
-- um operando possui um determinado tipo, 
-- retornando esse tipo caso verdadeiro e Undefined 
-- caso contrario.
is :: Exp -> Type -> Type
is e t 
 | (baseType e == t) = t
 | otherwise = Undefined

