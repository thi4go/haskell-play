module Tests where 

import Test.HUnit 

import Language
import Interpreter

exp1 = Let "x" (IConst 3) (IConst 4)
exp2 = Let "x" (IConst 3) (Add (RefId "x") (IConst 4))
exp3 = Let "x" (IConst 3) (Let "y" (IConst 4) (Add (RefId "x") (RefId "y")))
exp4 = Let "x" (IConst 3) (Let "x" (IConst 4) (Add (RefId "x") (RefId "x")))
exp5 = Let "x" (IConst 3) 
               (Add (Let "x" (IConst 4) (Add (RefId "x") (IConst 3))) 
                    (RefId "x"))
exp6 = Let "x" (IConst 3) 
               (Add (Let "y" (IConst 5) (Add (RefId "y") (IConst 4))) (RefId "y"))

test1 = TestCase (assertEqual "Let x = 3 in 4" (IntValue 4) (eval exp1 []))
test2 = TestCase (assertEqual "Let x = 3 in x + 4" (IntValue 7) (eval exp2 []))
test3 = TestCase (assertEqual "Let x = 3 in Let y = 4 in x + y" (IntValue 7) (eval exp3 []))

-- esse proximo teste deixa evidente que a nossa implementacao 
-- Let suporta bem uma nocao de escopo; permitindo 
-- lets aninhados. 
test4 = TestCase (assertEqual "Let x = 3 in Let x = 4 in x + x" (IntValue 8) (eval exp4 []))

test5 = TestCase (assertEqual "Let x = 3 in Add (Let x = 4 in x + 3) x" (IntValue 10) (eval exp5 []))
test6 = TestCase (assertEqual "Let x = 3 in Add (Let y = 4 in y + 4) y" (IntValue 12) (eval exp6 []))

tests = TestList [ TestLabel "test1" test1
                 , TestLabel "test2" test2
                 , TestLabel "test3" test3
                 , TestLabel "test4" test4
                 , TestLabel "test5" test5
                 ]
