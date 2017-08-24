module Coeffs where

data Exp = Var
			| Const Int
			| Add Exp Exp
			| Mult Exp Exp
	deriving(Show, Eq)

data TInt = I Int
	| L [Int]

eval :: Exp -> Int -> Int
eval Var x = x
eval (Const b) _ = b
eval (Add lhs rhs) x = (eval lhs x) + (eval rhs x)
eval (Mult lhs rhs) x = (eval lhs x) + (eval rhs x)

coeffs :: Exp -> [Int]
coeffs Var = [0, 1]
coeffs (Const b) = [b]
coeffs (Add l r) = undefined
coeffs (Mult l r) = undefined

coeffs_state :: Exp -> [Int] -> [Int]
coeffs_state _ _ = undefined

coeffs_single_state :: Exp -> Int -> Int
coeffs_single_state (Mult l r) i = 
	undefined
		
isVar :: Exp -> Bool
isVar exp =	case exp of
				Var -> True
				otherwise -> False
	

hasVar :: Exp -> Bool
hasVar Var = True
hasVar (Const _) = False
hasVar (Add l r) = (hasVar l) || (hasVar r)
hasVar (Mult l r) = (hasVar l) || (hasVar r)

counter :: Exp -> Int
counter Var = 1
counter (Const _) = 0
counter (Add l r) = counter l + counter r
counter (Mult l r) = counter l + counter r

