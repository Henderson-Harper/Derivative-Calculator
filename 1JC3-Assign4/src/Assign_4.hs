{-# LANGUAGE InstanceSigs #-}
{-|
Module      : 1JC3-Assign4.Assign_4.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 4 - McMaster CS 1JC3 2024
-}
module Assign_4 where

import Test.QuickCheck
import Data.Void (vacuous)
import qualified GHC.Float as Floating

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: Henderson Harper
-- Date: November 30th, 2024 + Revisions after
macid :: String
macid = "not telling you :)"

{- --------------------------------------------------------------------
 - Datatype: MathExpr
 - --------------------------------------------------------------------
 - Description: An Abstract Syntax Tree (AST) for encoding mathematical
 -              expressions
 - Example: The expression
 -                (abs (2*X + 1)) ^ 3
 -          can be encoded as
 -                Power 3 (Func1 Abs
 -                              (Func2 Add (Func2 Mult (Coef 2) X)
 -                                         (Coef 1)))
 - --------------------------------------------------------------------
 -}
data MathExpr a =
    X
  | Coef a
  | Add (MathExpr a) (MathExpr a)
  | Mult (MathExpr a) (MathExpr a)
  | Power (MathExpr a) Int
  | Cos (MathExpr a)
  | Sin (MathExpr a)
  | Abs (MathExpr a)
  deriving (Eq,Show,Read)

{- -----------------------------------------------------------------
 - eval
 - -----------------------------------------------------------------
 - Description:
 -    Takes in a MathExpr a as input and a value of type a
-     and returns the value of the expression at the given value
 -}
eval :: (Floating a, Eq a) => MathExpr a -> a -> a
eval X v = v
eval (Coef x) v = x
eval (Add a b) v = eval a v + eval b v
eval (Mult a b) v = eval a v * eval b v
eval (Power a x) v = eval a v ^^ x
eval (Cos x) v = cos (eval x v)
eval (Sin x) v = sin (eval x v)
eval (Abs x) v = abs (eval x v)

{- -----------------------------------------------------------------
 - instance Num a => Num (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    The implementation of the Num type class for the MathExpr type
 -}
instance Num a => Num (MathExpr a) where
  x + y         = Add x y
  x * y         = Mult x y
  negate x      = Mult x (Coef (-1))
  abs x         = Abs x
  fromInteger i = Coef (fromInteger i)
  signum _      = error "signum is left un-implemented"

{- -----------------------------------------------------------------
 - instance Fractional a => Fractional (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    The implementation of the Fractional type class for the MathExpr type
 -}
instance Fractional a => Fractional (MathExpr a) where
  recip e        = Power e (-1)
  fromRational e = Coef (fromRational e)

{- -----------------------------------------------------------------
 - instance Floating a => Floating (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    The implementation of the Floating type class for the MathExpr type
 -}
instance Floating a => Floating (MathExpr a) where
  pi      = Coef pi
  sin     = Sin
  cos     = Cos
  log     = error "log is left un-implemented"
  asin _  = error "asin is left un-implemented"
  acos _  = error "acos is left un-implemented"
  atan _  = error "atan is left un-implemented"
  sinh _  = error "sinh is left un-implemented"
  cosh _  = error "cosh is left un-implemented"
  tanh _  = error "tanh is left un-implemented"
  asinh _ = error "asinh is left un-implemented"
  acosh _ = error "acosh is left un-implemented"
  atanh _ = error "atanh is left un-implemented"
  exp _   = error "exp is left un-implemented"
  sqrt _  = error "sqrt is left un-implemented"

{------------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description:
 -    Takes a MathExpr as input and returns the derivative of the MathExpr as output
 -    Note that the derivative is not simplified
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X = 1
diff (Coef _) = 0
diff (Add a b) = Add (diff a)  (diff b)
diff (Mult a b) = Add (Mult (diff a) b) (Mult a (diff b))
diff (Power a b) = Mult (Mult (fromInteger (toInteger b)) (Power a (b-1))) (diff a)
diff (Cos a) = Mult (negate (Sin a)) (diff a)
diff (Sin a) = Mult (Cos a) (diff a)
diff (Abs a) = Mult (Mult a (recip (Abs a))) (diff a)

{-----------------------------------------------------------------
 - pretty
 - -----------------------------------------------------------------
 - Description:
 -    Takes in a MathExpr as input and returns a pretty-ified (more legible) version as output
 -}
-- NOTE: you'll have to test pretty yourself
pretty :: (Show a) => MathExpr a -> String
pretty X = "X"
pretty (Coef c) = "(" ++ show c ++ ")"
pretty (Add u0 u1) = "(" ++ pretty u0 ++ " + " ++ pretty u1 ++ ")"
pretty (Mult u0 u1) = "(" ++ pretty u0 ++ " * " ++ pretty u1 ++ ")"
pretty (Power u0 d) = "(" ++ pretty u0 ++ " ^^ " ++ show d ++ ")"
pretty (Cos u0) = "cos(" ++ pretty u0 ++ ")"
pretty (Sin u0) = "sin(" ++ pretty u0 ++ ")"
pretty (Abs u0) = "abs(" ++ pretty u0 ++ ")"

{-----------------------------------------------------------------
 - simplify
 - -----------------------------------------------------------------
 - Description:
 -    Takes in a MathExpr as input and returns a simplifed MathExpr as output
 -}
simplify :: (Floating a, Eq a) => MathExpr a -> MathExpr a

simplify (Add (Coef a) (Coef b)) = Coef (a+b)
simplify (Mult (Coef a) (Coef b)) = Coef (a*b)
simplify (Power (Coef a) b) = Coef (a ^^ b)


simplify (Add c1 (Coef 0)) = simplify c1
simplify (Add (Coef 0) c1) = simplify c1
simplify (Mult _ (Coef 0)) = Coef 0
simplify (Mult (Coef 0) _) = Coef 0
simplify (Mult a (Coef 1)) = a
simplify (Mult (Coef 1) a) = a
simplify (Power c1 1) = simplify c1
simplify (Power c1 0) = Coef 1

simplify (Add a b)
  | Add (simplify a) (simplify b) == Add a b = Add a b
  | otherwise = simplify (Add (simplify a) (simplify b))
simplify (Mult a b)
  | Mult (simplify a) (simplify b) == Mult a b = Mult a b
  | otherwise = simplify (Mult (simplify a) (simplify b))

simplify x = x

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------

Function: eval
Test Case Number: 1
Input: eval X (-5)
Expected Output: -5.0
Actual Output: -5.0
Rationale: testing eval with X

Function: eval
Test Case Number: 2
Input: eval (Coef 3) 1738
Expected Output: 3.0
Actual Output: 3.0
Rationale: testing eval with Coef constructor

Function: eval
Test Case Number: 3
Input: eval (Add X 3) 14
Expected Output: 17.0
Actual Output: 17.0
Rationale: testing eval with Add (im just going through every constructor of MathExpr first)

Function: eval
Test Case Number: 4
Input: eval (Mult (Add X X) (-4)) (-8)
Expected Output: 64.0
Actual Output: 64.0
Rationale: testing eval with Mult (and some nested stuff and negatives)

Function: eval
Test Case Number: 5
Input: eval (Cos X) (pi)
Expected Output: -1.0
Actual Output: -1.0
Rationale: testing eval with Cos

Function: eval 
Test Case Number: 6
Input: eval (Sin X) (pi/4)
Expected Output: 0.707... (sqrt2 / 2)
Actual Output: 0.7071067811865475
Rationale: testing eval with Sin

Function: eval
Test Case Number: 7
Input: eval Abs(Add X (Coef (-3))) (-4)
Expected Output: 7.0
Actual Output: 7.0
Rationale: testing eval with Abs

Function: diff
Test Case Number: 1
Input: diff X
Expected Output: Coef 1.0
Actual Output: Coef 1.0
Rationale: testing diff with the X constructor

Function: diff
Test Case Number: 2
Input: diff (Coef 12345)
Expected Output: Coef 0.0
Actual Output: Coef 0.0
Rationale: testing diff with the Coef constructor

Function: diff
Test Case Number: 3
Input: diff (Add (Coef 14) X)
Expected Output: Add (Coef 0.0) (Coef 1.0)
Actual Output: Add (Coef 0.0) (Coef 1.0)
Rationale: testing diff with Add (the lack of simplification is fine... right?)

Function: diff
Test Case Number: 4
Input: diff (Mult X (Coef (-5)))
Expected Output: Add (Mult (Coef 1.0) (Coef (-5.0))) (Mult X (Coef 0.0))
Actual Output: Add (Mult (Coef 1.0) (Coef (-5.0))) (Mult X (Coef 0.0))
Rationale: testing diff with Mult (product rule)

Function: diff
Test Case Number: 5
Input: diff (Power (Add X (Coef 3)) 5)
Expected Output: Mult (Mult (Coef 5.0) (Power (Add X (Coef 3.0)) 4)) (Add (Coef 1.0) (Coef 0.0))
Actual Output: Mult (Mult (Coef 5.0) (Power (Add X (Coef 3.0)) 4)) (Add (Coef 1.0) (Coef 0.0))
Rationale: testing power rule (with chain rule as well)

Function: diff
Test Case Number: 6
Input: diff (Cos (Add X X))
Expected Output: Mult (Mult (Sin (Add X X)) (Coef (-1))) (Add (Coef 1.0) (Coef 1.0))
Actual Output: Mult (Mult (Sin (Add X X)) (Coef (-1))) (Add (Coef 1.0) (Coef 1.0))
Rationale: testing cosine rule (with chain rule as well)

Function: diff
Test Case Number: 7
Input: diff (Sin (Coef 3.0))
Expected Output: Mult (Cos (Coef 3.0)) (Coef 0.0)
Actual Output: Mult (Cos (Coef 3.0)) (Coef 0.0)
Rationale: testing sine rule (with chain rule as well)

Function: diff
Test Case Number: 8
Input: diff (Abs (Add X (Coef (-54))))
Expected Output: Mult (Mult (Add X (Coef (-54.0))) (Power (Abs (Add X (Coef (-54)))) (-1))) (Add (Coef 1.0) (Coef 0.0))
Actual Output: Mult (Mult (Add X (Coef (-54.0))) (Power (Abs (Add X (Coef (-54)))) (-1))) (Add (Coef 1.0) (Coef 0.0))
Rationale: testing absolute value rule (with chian rule as well) 

Function: pretty
Test Case Number: 1
Input: pretty X
Expected Output: "X"
Actual Output: "X"
Output returns input: Yes
Rationale: testing pretty with the case of X (we are going to go through all the cases)

Function: pretty
Test Case Number: 2
Input: pretty (Coef (-1432))
Expected Output: "(-1432)"
Actual Output: "(-1432)"
Output returns input: No (but this makes sense because it sees this as a Num type)
Rationale: testing pretty with the case of Coef

Function: pretty
Test Case Number: 3
Input: pretty (Add X (Coef 13.43))
Expected Output: "(X + (13.43))"
Actual Output: "(X + (13.43))"
Output returns input: Yes
Rationale: testing pretty with the case of Add

Function: pretty
Test Case Number: 4
Input: pretty (Mult (Add X X) (Add (Coef 3.45) X))
Expected Output: "((X + X) * ((3.45) + X))"
Actual Output: "((X + X) * ((3.45) + X))"
Output returns input: Yes
Rationale: testing pretty with the case of Mult

Function: pretty
Test Case Number: 5
Input: pretty (Power (Add X (Coef 0)) 0)
Expected Output: "((X + (0)) ^^ 0)"
Actual Output: "((X + (0)) ^^ 0)"
Output returns input: No (but this makes sense because we don't have an implementation of ^^ for the MathExpr type)
Rationale: testing pretty with the case of Power

Function: pretty
Test Case Number: 6
Input: pretty (Cos (diff X))
Expected Output: "cos((1.0))"
Actual Output: "cos((1.0))"
Output returns input: No (this makes sense because it just evaluates  directly, 
                      if we do cos(X+X) we get Cos (Add X X) which is right so its fine)
Rationale: testing pretty with the case of Cos

Function: pretty
Test Case Number: 7
Input: pretty (Sin (eval X 1))
Expected Output: "sin((1.0))"
Actual Output: "sin((1.0))"
Output returns input: No (this also just evaluates directly,
                          if we do sin(X+X) we get Sin (Add X X) which is right so its also fine)
Rationale: testing pretty with the case of Sin

Function: pretty
Test Case Number: 8
Input: pretty (Abs(Add X (Coef (22/7)))
Expected Output: "abs((X + (3.142857142857...)))"
Actual Output: "abs((X + (3.142857142857143)))"
Output returns input: Kinda (3.14285... doesn't convert back to a fraction,
                             otherwise it is the same so its fine)
Rationale: testing pretty with the case of Abs
 -}

infix 4 =~
(=~) :: (Floating a,Ord a) => a -> a -> Bool
x =~ y = abs (x - y) <= 1e-4

{- EXAMPLE
 - Function: eval
 - Property: eval (Func2 Add (Coef x) X) y is correct for all x,y
 - Actual Test Result: Pass
 -}
evalProp0 :: (Float,Float) -> Bool
evalProp0 (x,y) = (x + y) =~ eval (Add (Coef x) X) y

runEvalProp0 :: IO ()
runEvalProp0 = quickCheck  evalProp0

{- 
 - Function: eval
 - Property: eval (Mult (Coef x) (Coef y)) z is correct for all x,y,z
   - Basically just testing multiplication of two coefficients
   - z is a dummy value to make sure that it value doesn't affect the result
 - Actual Test Result: Pass
-}

evalProp1 :: (Float,Float,Float) -> Bool
evalProp1 (x,y,z) = (x*y) =~ eval (Mult (Coef x) (Coef y)) z

runEvalProp1 :: IO ()
runEvalProp1 = quickCheck evalProp1

{-
 - Function: diff
 - Property: eval (diff (Mult (Coef c) X)) x is correct for all c,x
   - Basically testing that the derivative of c*X is always c at any point on the function
 - Actual Test Result: Pass
-}

diffProp1 :: (Float, Float) -> Bool
diffProp1 (c,x) = eval (diff (Mult (Coef c) X)) x =~ c

runDiffProp1 :: IO ()
runDiffProp1 = quickCheck diffProp1