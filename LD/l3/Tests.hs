module Tests where 

import Test.HUnit

import Language
import Interpreter

inc = FuncDecl "inc" ["x"] (Add (RefId "x") (IConst 1))
soma = FuncDecl "soma" ["x", "y"] (Add (RefId "x") (RefId "y"))
foo = FuncDecl "foo" ["p"] (RefId "n")


test1 = TestCase (assertEqual "inc 5" (IntValue 6) (eval (App "inc" [(IConst 5)]) [] [inc]))
test2 = TestCase (assertEqual "soma 3 4" (IntValue 15) (eval (App "soma" [(IConst 5),(IConst 10)]) [] [soma]))
test3 = TestCase (assertEqual "Let n = 5 in f 3, onde foo p = n" (IntValue 5) (eval (Let "n" (IConst 5) (App "foo" [(IConst 3)])) [] [foo]))

allTests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3]


