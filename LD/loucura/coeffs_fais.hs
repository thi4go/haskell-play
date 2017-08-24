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
coeffs (Add lhs rhs) = 
	if (greater lhs rhs) == rhs
	then (coeffs lhs) ++ (coeffs rhs)
	else (coeffs rhs) ++ (coeffs rhs)
coeffs (Mult lhs rhs) =
	if (greater lhs rhs) == rhs
	then (coeffs lhs) ++ (coeffs rhs)
	else (coeffs rhs) ++ (coeffs rhs)

greater :: Exp -> Exp -> Exp
greater exp1 exp2 = 
	if (counter exp1) > (counter exp2)
	then exp1
	else exp2

{-theother :: Exp -> Exp -> Exp-}
{-theother exp1 exp2 = -}
	{-if greater exp1 exp2 == exp1-}
	{-then exp2-}
	{-else exp1-}

total :: Exp -> TInt
total Var = I 1
total (Const x) = I x
total (Add l r) = 
	if (not (hasVar l) && not (hasVar r))
	then sumT (total l) (total r) 
	else  if ((hasVar l) && (hasVar r))
		then undefined -- L [0, sumT r l]
		else undefined
total (Mult l r) = undefined

hasVar :: Exp -> Bool
hasVar Var = True
hasVar (Const x) = False
hasVar (Add l r) = (hasVar l) && (hasVar r)
hasVar (Mult l r) = (hasVar l) && (hasVar r)

sumT :: TInt -> TInt -> TInt
sumT (I a) (I b) = I (a + b)

counter :: Exp -> Int
counter Var = 1
counter (Const x) = 0
counter (Add lhs rhs) = (counter lhs) + (counter rhs)
counter (Mult lhs rhs) = (counter lhs) + (counter rhs)
