module Main where

import Arithmatic (modInv)

data Variable = Variable {typeOfVar :: Value,
                          nameOfVar :: String}
                deriving (Eq, Show)

data Value = GElement Integer Integer  -- group-side, number
           | Group Integer  -- size
           | Natural
           | FElement Integer Integer  -- field-size, number
           | Field Integer  -- size
           | Prime  -- PrimePower?
           | Lambda Variable Expression
           | Function Value Value
           | Type
             deriving (Eq, Show)
typeOfValue GElement m _ = Group m
typeOfValue Group _ = Natural
typeOfValue Natural = Type
typeOfValue FElement m _ = Field m
typeOfValue Field _ = Prime
typeOfValue Prime = Type
typeOfValue Lambda v e = Function (typeOfVar v) (typeOf [] e)  -- Wrong! need an evaluati on context!
typeOfValue Function _ _ = Type
typeOfValue Type = Type

data Expression = BaseValue Value      -- v
                | BaseVariable String  -- x  -- not Variable?
                | TypeOf Expression    -- v->v
                | Negative Expression  -- a in {Group, Field} => a->a
                | Addition Expression Expression  -- a in {Group, Natural, Field} => a->a->a
                | Inverse Expression   -- Field x -> Field x
                | Multiplication Expression Expression  -- Field x -> Field x
                | Modulo Expression Expression  -- a in {Natural, Prime}, b in {Group, Natural, Field} => a->b->a
                | Application Expression Expression  -- function, value
                | Let Variable Expression Expression  -- Let var=exp in body
                | Conditional Expression Expression Expression  -- predicate, true-path, false-path
                  deriving (Show)

type TypeContext = [Variable]
lookupType :: TypeContext -> String -> Type
lookupType (v:gamma) n = if n == (nameOfVar v) then typeOfVar v else lookupType gamma n
lookupType [] _ = undefined  -- Gotta represent failure somehow!

typeOf :: TypeContext -> Expression -> Value
typeOf gamma e = case e of    -- Many of these probably need to be more agressive in failure.
    BaseValue v = typeOfValue v
    BaseVariable name -> gamma `lookupType` name
    TypeOf exp -> typeOfValue $ tInG exp
    Negative exp -> tInG exp
    Addition expL expR -> let t = tInG expL
                          in if t == (tInG expR) then t else undefined
    Inverse exp -> tInG exp
    Multiplication expL expR -> let t = tInG expL
                                in if t == (tInG expR) then t else undefined
    Application func val -> let tval = tInG val
                                Function from to = tInG func
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
evalExp gamma e = case e of
    val@(BaseValue _) -> val
    BaseVariable name -> gamma `lookupVariable` name
    -- Negation cases
    Negative (BaseValue (Element prime val)) -> BaseValue $ Element prime ((-val) `mod` prime)
    Negative (BaseValue _) -> undefined
    Negative exp -> Negative $ eInG exp
    -- Inverse cases
    Inverse (BaseValue (Element prime val)) -> BaseValue $ maybe undefined (Element prime) (val `modInv` prime)
    Inverse (BaseValue _) -> undefined
    Inverse exp -> Inverse $ eInG exp
    -- Addition cases
    Addition (BaseValue (Element prime val1)) (BaseValue (Element _ val2)) -> BaseValue $ Element prime ((val1 + val2) `mod` prime)
    Addition exp1@(BaseValue _) exp2 -> Addition exp1 (eInG exp2)
    Addition exp1 exp2@(BaseValue _) -> Addition (eInG exp1) exp2  -- No explicit failure for Lambda!?
    -- Multiplicaiton cases
    Multiplication (BaseValue (Element prime val1)) (BaseValue (Element _ val2)) -> BaseValue $ Element prime ((val1 * val2) `mod` prime)
    Multiplication exp1@(BaseValue _) exp2 -> Multiplication exp1 (eInG exp2)
    Multiplication exp1 exp2@(BaseValue _) -> Multiplication (eInG exp1) exp2  -- No explicit failure for Lambda!?
    -- Application cases
    Application (BaseValue (Lambda var body)) arg -> evalExp ((var, arg):gamma) body
    Application (BaseValue _) _ -> undefined
    Application funcExp arg -> Application (eInG funcExp) arg
    -- Let cases (Let is just sugar!)
    Let var val body -> Application (BaseValue (Lambda var body)) val
    -- Conditional cases
    Conditional (BaseValue (Element 2 b)) expT expF -> eInG $ if 1 == b then expT else expF
    Conditional (BaseValue _) _ _ -> undefined
    Conditional predicate expT expF -> Conditional (eInG predicate) expT expF
    where eInG = evalExp gamma

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



( \x:Int -> as_element[x + 1](1 + 5) ) : (Mod[x + 1]) : Group : Type

( x:f() + y:g() ) 

( Let x = as_element[x](x) In as_element[x](x) ) : Mod[x]


