module Evaluation where

import Arithmatic (modInv)
import Nouns

typeOfValue :: TypeContext -> Value -> Value
typeOfValue _ GroupElement m _ = GroupType m
typeOfValue _ GroupType _ = GroupClass
typeOfValue _ GroupClass = Type
typeOfValue _ FieldElement m _ = FieldType m
typeOfValue _ FieldType _ = FieldClass
typeOfValue _ FieldClass = Type
typeOfValue gamma Lambda v e = Function (typeOfVar v) (typeOf v:gamma e)
typeOfValue _ Function _ _ = Type
typeOfValue _ Type = Type

type TypeContext = [Variable]
lookupType :: TypeContext -> String -> Type
lookupType (v:gamma) n = if n == (nameOfVar v)
                         then typeOfVar v
                         else lookupType gamma n
lookupType [] _ = undefined  -- Gotta represent failure somehow!

typeOf :: TypeContext -> Expression -> Value
typeOf gamma e = case e of    -- maybe be more agressive in failure
    BaseValue v = typeOfValue gamma v
    BaseVariable name -> gamma `lookupType` name
    TypeOf exp -> typeOfValue $ tInG exp
    Negative exp -> case tInG exp of
        t@(GroupType _) -> t
        t@(FieldType _) -> t
        _ -> undefined
    Addition expL expR -> case assuming (== tInG expR) (tInG expL) of
        t@(GroupType _) -> t
        t@(GroupClass) -> t
        t@(FieldType _) -> t
        _ -> undefined
    Inverse exp -> case tInG exp of
        t@(FieldType _) -> t
        _ -> undefined
    Multiplication expL expR -> case assuming (== tInG expR) (tInG expL) of
        t@(FieldType _) -> t
        _ -> undefined
    AsElement base exp -> let checkExpType = const id (  -- lame startegy
                              case tInG exp of
                                  GroupType _ -> ()
                                  FieldType _ -> ()
                                  Natural -> ()
                                 _ -> undefined
                          )
                          in case base of  -- We need to evaluate base!
                              b@(GroupType _) -> checkExpType b
                              b@(FieldType _) -> checkExpType b
                              _ -> undefined
    AsNatural exp -> case tInG exp of
        t@(FieldType _) -> t
        t@(FieldClass) -> t
        t@(GroupType _) -> t
        _ -> undefined
    Application func val -> let Function from to = tInG func  -- could fail
                            in if tInG val == from then to else undefined
    Let var val body -> tInG $ Application (BaseValue (Lambda var body)) val
    Conditional pred a b -> let testBranchTypes = (== tInG b)
                                testPredType = (&& (tInG pred == Field 2))
                            in assuming (testPredType . testBranchTypes) tInG a
    where tInG = typeOf gamma
          assuming predicate value = if (predicate value)
                                     then value
                                     else undefined

type EvalContext = [(Variable, Expression)]
lookupVariable :: EvalContext -> String -> Expression
lookupVariable ((var, val):gamma) n = if n == (nameOfVar var)
                                      then val
                                      else lookupVariable gamma n
lookupVariable [] _ = undefined

evalExp :: EvalContext -> Expression -> Expression
evalExp gamma e = case e of
    val@(BaseValue _) -> val
    BaseVariable name -> gamma `lookupVariable` name
    -- Negation cases
    Negative (BaseValue element) ->
        let (kind, size, num) = case element of
                GroupElement s n -> (GroupElement, s, n)
                FieldElement s n -> (FieldElement, s, n)
                _ -> undefined
        in kind size $ (-num) `mod` size
    Negative exp -> Negative $ eInG exp
    -- Inverse cases
    Inverse (BaseValue (FieldElement size val)) ->
        BaseValue $ maybe undefined (FieldElement size) (val `modInv` size)
    Inverse (BaseValue _) -> undefined
    Inverse exp -> Inverse $ eInG exp
    -- Addition cases
    Addition (BaseValue valA) (BaseValue valB) ->
        let (kind, size, numA, numB) = case (valA, valB) of
                (GroupElement s a, GroupElement _ b) -> (GroupElement, s, a, b)
                (FieldElement s a, FieldElement _ b) -> (FieldElement, s, a, b)
                _ -> undefined
        in kind size $ (numA + numB) `mod` size
    Addition exp1@(BaseValue _) exp2 -> Addition exp1 (eInG exp2)
    Addition exp1 exp2 -> Addition (eInG exp1) exp2
    -- Multiplicaiton cases
    Multiplicaiton (BaseValue valA) (BaseValue valB) ->
        let (kind, size, numA, numB = case (valA, valB) of
                (FieldElement s a, FieldElement _ b) -> (FieldElement, s, a, b)
                _ -> undefined
        in kind size $ (numA * numB) `mod` size
    Multiplication exp1@(BaseValue _) exp2 -> Multiplication exp1 (eInG exp2)
    Multiplication exp1 exp2 -> Multiplication (eInG exp1) exp2
    -- AsElement cases
    -- AsNatural cases
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




