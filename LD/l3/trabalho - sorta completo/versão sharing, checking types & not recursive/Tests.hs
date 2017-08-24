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

reft = Let "x" (IConst 3) (Let "f" (Fun [("y", IntType)] [] (Add (RefId "y") (RefId "x")) ) (reftest)) --Note que agora não tem mais como usar f em let, pois não temos tipos funcionais
reftest = Let "x" (IConst 299) (App (RefId "f") [(IConst 6)])
reft2 = Let "x" (IConst 1) (Let "f" (Fun [("y", IntType)] [] (Add (RefId "y") (RefId "x"))) (App (RefId "f") [(IConst 3)]))

--App (Fun [("x",IntType)] [] (App (Fun [("f",FunType [IntType] IntType)] [("x",IConst 1)] (App (RefId "f") [IConst 3])) [Fun [("y",IntType)] [] (Add (RefId "y") (RefId "x"))])) [IConst 1]

kk = preprocess (Let "f" (Fun [] [("y", IConst 1)] (RefId "y")) (RefId "f")) []
kk1 = Fun [] [("y", IConst 3)] (RefId "y")

--App (Fun [("f",FunType [IntType] IntType)] (RefId "f") []) [Fun [("x",IntType)] (RefId "x") []]

--(App (RefId "f") [IConst 6])) [IConst 299])) )) [("x", 299), ("f", Fun [("y",IntType)] (Add (RefId "y") (RefId "x"))), ("x", IConst 3)]

--dble = Let "double" (Fun [("x", IntType)] (Add (RefId "x") (RefId "x"))) (App (RefId "double") [(IConst 5)])

--lazytst = Let "x" (Add (IConst 4) (IConst 5)) (Let "y" (Add (RefId "x") (RefId "x")) (Let "z" (RefId "y") (RefId "z")))
--lazytst2 = Let "x" (Add (IConst 4) (IConst 5)) (Let "z" (RefId "x") (RefId "z"))

--pretst = Let "f" (Fun [("x", IntType)] (Add (RefId "x") (IConst 1))) (App (RefId "f") [(IConst 3)])

{-reftest2 = Let "x" (IConst 3) (Fun [("y", IntType)] (Add (RefId "x") (RefId "y")))
fattest = (Fun [("x", IntType)] (IfTEls (Comp (RefId "x") (IConst 0))
								(IConst 1)
								(Mult (RefId "x") (App (fattest) [(Sub (RefId "x") (IConst 1))]))))

fun1 = (Fun [("x", IntType)] (RefId "x"))
fun2 = (Fun [("x", IntType)] (Add (IConst 5) (RefId "x")))

funtest = (App fun1 [(App fun2 [(IConst 3)])])
funtest2 = Let "x" (IConst 1) (App fun1 [(Add (IConst 3) (RefId "x"))])
-}