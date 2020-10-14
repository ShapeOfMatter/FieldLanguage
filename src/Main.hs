module Main where

import Arithmatic (modInv)

data Type = Field Integer  -- prime/modulo 
          | Function Type Type  -- from, to

lambdaType :: Type -> (Type, Type)
lambdaType (Function from to) = (from, to)
lambdaType _ = undefined  -- There's gonna be a lot of these partial functions. It's not a good system.


data Variable = Variable {typeOfVar :: Type,
                          nameOfVar :: String} 

data Value = Value Integer Integer  -- field, number
           | Lambda Variable Expression

trueValue = Value 2 1  -- Do we need these here? I think we need them for the definition of IF...
falseValue = Value 2 0

data Expression = BaseValue Value
                | BaseVariable String  -- not Variable?
                | Negative Expression
                | Inverse Expression
                | Addition Expression Expression
                | Multiplication Expression Expression
                | Application Expression Expression
                | Let Variable Expression Expression
                | Conditional Expression Expression Expression

newtype TypeContext = TypeContext [Variable]
lookupType :: TypeContext -> String -> Type
lookupType v:gamma n = if n == (nameOfVar v) then typeOfVar v else lookupType gamma n
lookupType [] = undefined  -- Gotta represent failure somehow!

typeOf :: TypeContext -> Expression -> Type
typeOf gamma = tInG
    where tInG (BaseValue (Value prime _)) = Field prime,
          tInG (BaseValue (Lambda var body)) = Function (typeOfVar var) $ typeOf (var:gamma) body
          tInG (BaseVariable name) = gamma `lookupType` name
          tInG (Negative exp) = tInG exp
          tInG (Inverse exp) = tInG exp
          tInG (Addition expL expR) = let t = tIng expL
                                      in if t == (tInG expR) then t else undefined
          tIng (Multiplication expL expr) = let t = tIng expL
                                            in if t == (tInG expR) then t else undefined
          tInG (Application func val) = let tval = tInG val
                                            (from, to) = lambdaType $ tIng func
                                        in if tval == from then to else undefined  -- TODO: Better error reporting
          tInG (Let var val body) = tInG $ Application (Lambda var body) val
          tInG (Conditional pred a b) = let ta = tInG a
                                        in if (tInG pred) == Field 2 && ta == (tInG b) then ta else undefined

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




