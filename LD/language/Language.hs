module Language where

type Id = String
type Args = [Exp]

data Exp = 	IConst Int
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
	deriving(Show)

data Type =	IntType
		  | BooleanType
		  | Undefined
	deriving(Show, Eq)

data Value =  IntValue Int
		  	| BooleanValue Bool
	deriving(Show, Eq)

{-args' = [IConst 1, IConst 2, IConst 3]-}

type FormalArgs = [Id]
type Binding = (Id, Exp)

type Env = [Binding]

