module Tests where 

import Language
import Interpreter 

import Test.HUnit 

add00, add34, add3_4, add4_4 :: Exp
add00  = Add (IConst 0) (IConst 0)
add34  = Add (IConst 3) (IConst 4)
add3_4 = Add (IConst 3) (IConst (-4))
add4_4 = Add (IConst 4) (IConst (-4))
add4add34 = Add (IConst 3) add34

test0 = TestCase (assertEqual "Add 0 0" 0 (eval add00))
test1 = TestCase (assertEqual "Add 3 4" 7 (eval add34))
test2 = TestCase (assertEqual "Add 3 (-4)" (-1) (eval add3_4))
test3 = TestCase (assertEqual "Add 4 (-4)" 0 (eval add4_4))
test4 = TestCase (assertEqual "Add 4 (3+4)" 10 (eval add4add34))

allTests = TestList [TestLabel "test0" test0
                    ,TestLabel "test1" test1
                    ,TestLabel "test2" test2
                    ,TestLabel "test3" test3
                    ,TestLabel "test4" test4
                    ]


