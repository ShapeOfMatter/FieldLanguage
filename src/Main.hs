module Main where

import Arithmatic (modInv)

data Type = Field Integer  -- prime/modulo 
          | Function Type Type  -- from, to

data Value = Value Integer Integer  -- prime, number
           | Negation Value
           | Inverse Value
           | Addition Value Value
           | Multiplication Value Value

data Expression = Variable String
                | Application Expression Expression
                | Lambda String Type Expression
                | Let String Expression Expression
                | Conditional Expression Expression Expression
                | Basic Value

typeOf :: Context -> Expression -> Type
typeOf gamma (Variable name) = gamma `lookupType` name
typeOf gamma (Application func val) = let tfunc = typeOf gamma func,
                                          tval = typeOf gamma val
                                          (from, to) = lambdaType tfunc  -- How?
                                      in if tval == from then to else undefined  -- TODO: Better error reporting
typeOf gamma (Lambda var tvar body) = Function tvar (typeof ((var, tvar, _):gamma) body)  -- meh?
typeOf gamma (Let var val body) = let tval = typeOf gamma val  -- I don't think we have to recurse here, but maybe it's clearer if we do?
                                  in typeOf gamma $ Application (Lambda var tval body) val
typeOf gamma (Conditional predicate a b) = let ta = typeOf gamma a
                                           in if (typeOf gamma predicate) == Field 2 && (typeOf gamma b) == ta
                                              then ta else undefined
typeOf _ (Basic val) = let Value prime _ = evalValue val  -- This doesn't work! doesn't validate anything!
                       in Field prime

evalExp :: Context -> Expression -> Expression
evalExp = undefined

evalValue :: Value -> Value
-- base cases: 
evalValue (Negation (Value p n)) = Value p ((-n) `mod` p))
evalValue (Inverse (Value p n)) = maybe undefined (Value p) (n `modInv` p)
evalValue (Addition (Value p n1) (Value _ n2) = Value p ((n1 + n2) `mod` p)
evalValue (Multiplicaiton (Value p n1) (Value _ n2) = Value p ((n1 * n2) `mod` p)
-- inductive cases:
evalValue (Negation v) = evalValue $ Negation $ evalValue v
evalValue (Inverse v) = evalValue $ Inverse $ evalValue v
evalValue (Addition v1 v2) = evalValue $ Addition (evalValue v1) (evalValue v2)
evalValue (Multiplication v1 v2) = evalValue $ Multiplication (evalValue v1) (evalValue v2)
-- extra base case:
evalValue v = v

newtype Context = Context [(String, Type, Expression)]

evalStep :: Context -> Expression -> Expression
evalStep gamma (Variable name) =  gamma `lookupValue` name
evalStep gamma (Application f a) = evalStep 

main :: IO ()i
main = putStrLn "Hello, Haskell!"




