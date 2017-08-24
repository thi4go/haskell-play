module Tests where

import Test.HUnit 

import Language
import Interpreter

exp1, exp2, exp3, exp4 :: Exp
exp1 = And(BConst False) (BConst True)
exp2 = Add (Add (IConst 3) (IConst 4)) (IConst 3)
exp3 = Not (BConst False)
exp4 = And (IConst 4) (BConst False)

test1 = TestCase (assertEqual "And False True" (BooleanValue False) (eval exp1))
test2 = TestCase (assertEqual "Add (Add 3 4) 3" (IntValue 10) (eval exp2))
test3 = TestCase (assertEqual "Not False" (BooleanValue True) (eval exp3))
test4 = TestCase (assertEqual "error expected" (undefined) (eval exp4))

allTests = TestList [TestLabel "test1" test1
                    ,TestLabel "test2" test2
                    ,TestLabel "test3" test3
                    ,TestLabel "test4" test4
                    ]

