
data Nat = Zero | Suc Nat
    deriving (Show)
  

data Bool = True | False

eq :: Nat -> Nat -> Bool
eq Zero Zero = True
eq Zero r    = False
eq l Zero    = False
eq (Suc l) (Suc r) = eq l r 



add :: Nat -> Nat -> Nat
add Zero r = r
add l Zero = l
add (Suc l) (Suc r) = Suc (Suc (add l r))



mul :: Nat -> Nat -> Nat
mul Zero r = Zero
mul l Zero = Zero
mul l (Suc r) = add l (mul l r)


sub :: Nat -> Nat -> Nat
sub Zero Zero = Zero
sub Zero r = error "lol"
sub l Zero = l
sub (Suc l) (Suc r) = sub l r


gt :: Nat -> Nat -> Bool
gt Zero Zero = False
gt l Zero    = True
gt Zero r    = False
gt (Suc l) (Suc r) = gt l r

lt l r = not (eq l r) && not (gt l r)

















