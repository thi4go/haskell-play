module Tests where 

--import Test.HUnit

import Language
import Interpreter
{-
inc = FuncDecl "inc" [("x", IntType)] (Add (RefId "x") (IConst 1))
soma = FuncDecl "soma" [("x", IntType), ("y", IntType)] (Add (RefId "x") (RefId "y"))
foo = FuncDecl "foo" [("p", IntType)] (RefId "n")


test1 = TestCase (assertEqual "inc 5" (IntValue 6) (eval (App "inc" [(IConst 5)]) [] [inc]))
test2 = TestCase (assertEqual "soma 3 4" (IntValue 15) (eval (App "soma" [(IConst 5),(IConst 10)]) [] [soma]))
test3 = TestCase (assertEqual "Let n = 5 in f 3, onde foo p = n" (IntValue 5) (eval (Let "n" (IConst 5) (App "foo" [(IConst 3)])) [] [foo]))

allTests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3]


lettest = (Let "x" (IConst 3)
		(Let "f" (Fun [("y", IntType)] (Sum (RefId "y") (RefId "x")
		Let "x" (IConst 5)
		App (Ref "f") (IConst 4)
-}

reftest = Let "x" (IConst 3) (Let "f" (Fun [("y", IntType)] (Add (RefId "y") (RefId "x"))) (Let "x" (IConst 10) (App (RefId "f") [(IConst 4)])))

fattest = (Fun [("x", IntType)] (IfTEls (Comp (RefId "x") (IConst 0))
								(IConst 1)
								(Mult (RefId "x") (App (fattest) [(Sub (RefId "x") (IConst 1))]))))
											
