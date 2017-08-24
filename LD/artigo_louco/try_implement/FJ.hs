module FJ where

data Exp = Var Id
	|Atr Exp
	|Meth Exps Exp
	|New Id Exps
	|Cast Exp Exp

type Exps = [Exp]
type Id = String
