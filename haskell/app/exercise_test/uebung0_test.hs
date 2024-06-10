module Uebung0_Test where

import Test.HUnit

test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))
test2 = TestCase (do (x,y) <- partA 3
                     assertEqual "for the first result of partA," 5 x
                     b <- partB y
                     assertBool ("(partB " ++ show y ++ ") failed") b)

-- Test
-- exponentTest :: Int -> Int -> Int -- receives two integers n & x
-- exponentTest n x
--   | n <= 0    = 1 -- base case for exponentiation
--   | otherwise = applyNtimes (* x) (n - 1) x