module Naturals where

data Nat = Zero
		 | Succ Nat
	deriving(Show)

instance Eq Nat where
	Zero == Zero = True
	Succ _ == Zero = False
	Zero == Succ _ = False
	Succ x == Succ y = x == y

listNat = [Zero, Succ Zero, Succ (Succ Zero)]
