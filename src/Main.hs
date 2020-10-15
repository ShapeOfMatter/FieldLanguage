module Main where

import Arithmatic (modInv)

data Type = Field Integer  -- prime/modulo 
          | Function Type Type  -- from, to
            deriving (Eq, Show)

lambdaType :: Type -> (Type, Type)
lambdaType (Function from to) = (from, to)
lambdaType _ = undefined  -- There's gonna be a lot of these partial functions. It's not a good system.


data Variable = Variable {typeOfVar :: Type,
                          nameOfVar :: String} 
                deriving (Eq, Show)

data Value = Element Integer Integer  -- field, number
           | Lambda Variable Expression
             deriving (Show)

trueElement = Element 2 1  -- Do we need these here? I think we need them for the definition of IF...
falseElement = Element 2 0

data Expression = BaseValue Value
                | BaseVariable String  -- not Variable?
                | Negative Expression
                | Inverse Expression
                | Addition Expression Expression
                | Multiplication Expression Expression
                | Application Expression Expression  -- function, value
                | Let Variable Expression Expression  -- Let var=exp in body
                | Conditional Expression Expression Expression  -- predicate, true-path, false-path
                  deriving (Show)

type TypeContext = [Variable]
lookupType :: TypeContext -> String -> Type
lookupType (v:gamma) n = if n == (nameOfVar v) then typeOfVar v else lookupType gamma n
lookupType [] _ = undefined  -- Gotta represent failure somehow!

typeOf :: TypeContext -> Expression -> Type
typeOf gamma e = case e of
    BaseValue (Element prime _) -> Field prime
    BaseValue (Lambda var body) -> Function (typeOfVar var) $ typeOf (var:gamma) body
    BaseVariable name -> gamma `lookupType` name
    Negative exp -> tInG exp
    Inverse exp -> tInG exp
    Addition expL expR -> let t = tInG expL
                          in if t == (tInG expR) then t else undefined
    Multiplication expL expR -> let t = tInG expL
                                in if t == (tInG expR) then t else undefined
    Application func val -> let tval = tInG val
                                (from, to) = lambdaType $ tInG func
                            in if tval == from then to else undefined  -- TODO: Better error reporting
    Let var val body -> tInG $ Application (BaseValue (Lambda var body)) val
    Conditional pred a b -> let ta = tInG a
                            in if (tInG pred) == Field 2 && ta == (tInG b) then ta else undefined
    where tInG = typeOf gamma

type EvalContext = [(Variable, Expression)]
lookupVariable :: EvalContext -> String -> Expression -- If we could make this stateful it'd be a huge boost in performance.
lookupVariable ((var, val):gamma) n = if n == (nameOfVar var) then val else lookupVariable gamma n
lookupVariable [] _ = undefined

evalExp :: EvalContext -> Expression -> Expression
evalExp gamma = eInG
    where eInG val@(BaseValue _) = val
          eInG (BaseVariable name) = gamma `lookupVariable` name
          -- Negation cases
          eInG (Negative (BaseValue (Element prime val))) = BaseValue $ Element prime ((-val) `mod` prime)
          eInG (Negative (BaseValue _)) = undefined
          eInG (Negative exp) = Negative $ eInG exp
          -- Inverse cases
          eInG (Inverse (BaseValue (Element prime val))) = BaseValue $ maybe undefined (Element prime) (val `modInv` prime)
          eInG (Inverse (BaseValue _)) = undefined
          eInG (Inverse exp) = Inverse $ eInG exp
          -- Addition cases
          eInG (Addition (BaseValue (Element prime val1)) (BaseValue (Element _ val2))) = BaseValue $ Element prime ((val1 + val2) `mod` prime)
          eInG (Addition exp1@(BaseValue _) exp2) = Addition exp1 (eInG exp2)
          eInG (Addition exp1 exp2@(BaseValue _)) = Addition (eInG exp1) exp2  -- No explicit failure for Lambda!?
          -- Multiplicaiton cases
          eInG (Multiplication (BaseValue (Element prime val1)) (BaseValue (Element _ val2))) = BaseValue $ Element prime ((val1 * val2) `mod` prime)
          eInG (Multiplication exp1@(BaseValue _) exp2) = Multiplication exp1 (eInG exp2)
          eInG (Multiplication exp1 exp2@(BaseValue _)) = Multiplication (eInG exp1) exp2  -- No explicit failure for Lambda!?
          -- Application cases
          eInG (Application (BaseValue (Lambda var body)) arg) = evalExp ((var, arg):gamma) body
          eInG (Application (BaseValue _) _) = undefined
          eInG (Application funcExp arg) = Application (eInG funcExp) arg
          -- Let cases (Let is just sugar!)
          eInG (Let var val body) = Application (BaseValue (Lambda var body)) val
          -- Conditional cases
          eInG (Conditional (BaseValue (Element 2 b)) expT expF) = eInG $ if 1 == b then expT else expF
          eInG (Conditional (BaseValue _) _ _) = undefined
          eInG (Conditional predicate expT expF) = Conditional (eInG predicate) expT expF

evaluateWith :: EvalContext -> Expression -> Value
evaluateWith _ (BaseValue val) = val
evaluateWith gamma exp = evaluateWith gamma $ evalExp gamma exp
evaluate = evaluateWith []




testExp = Application func five
          where t = Field 11
                x = BaseVariable "x"
                five = BaseValue $ Element 11 5
                func = BaseValue $ Lambda (Variable t "x") $ Addition x five

main :: IO ()
main = do putStrLn "Hello, Haskell!"
          print testExp
          print $ typeOf [] testExp
          print $ evalExp [] testExp
          print $ evaluate testExp




