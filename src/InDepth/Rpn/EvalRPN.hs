module InDepth.Rpn.EvalRPN where

import Control.Monad.State

{-
    Function evalRPN evaluates an expression given
    in the reversed polish notation (RPN, posix notation):
    
    evalRPN "2 3 +" ==> 5 (== "2 + 3")
    evalRPN "2 3 4 + *" ==> 14 (== 2 * (3 + 4))
    evalRPN "3 2 -" ==> 1 (== "3 - 2")
-}

type Stack = [Integer]
type EvalM = State Stack

push :: Integer -> EvalM ()
push x = modify (x:)

pop :: EvalM Integer
pop = do
    xs <- get
    put (tail xs)
    pure (head xs)

pop' :: EvalM Integer
pop' = state $ \(x:xs) -> (x, xs)