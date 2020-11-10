module Evaluation where

import Arithmatic (modInv)
import Nouns


isMinimal :: Expression -> Bool
isMinimal e = case e of
    (Type) -> True
    (Natural _) -> True
    (Naturals) -> True
    (GroupElement _ _) -> True
    (Group _) -> True
    (GroupClass) -> True
    (FieldElement _ _) -> True
    (Field _) -> True
    (FieldClass) -> True
    (Orange _ v) -> isMinimal v
    (Blue _ v) -> isMinimal b
    (Choice l r) -> isMinimal l && isMinimal r
    (Pair f s) -> isMinimal f && isMinimal s
    (Lambda name isA body) -> true -- ? 
    (Function from to) -> isMinimal from && isMinimal to
    _ -> False

lazyReductionStep :: Context -> Expression -> Expression
lazyReductionStep gamma expression = case expression of
    Type -> Type
    TypeOf exp -> eagerTypeOf exp
    Cast

typeOfExpression :: TypeContext -> Expression -> Expression
typeOfExpression _ GroupElement m _ = GroupType m
typeOfExpression _ GroupType _ = GroupClass
typeOfExpression _ GroupClass = Type
typeOfExpression _ FieldElement m _ = FieldType m
typeOfExpression _ FieldType _ = FieldClass
typeOfExpression _ FieldClass = Type
typeOfExpression gamma Orange bt ov = Choice (type) bt
typeOfExpression gamma Lambda v e = Function (typeOfVar v) (typeOf v:gamma e)
typeOfExpression _ Function _ _ = Type
typeOfExpression _ Type = Type

typeOf :: Context -> Expression -> Expression
typeOf gamma e = case e of    -- maybe be more agressive in failure
    BaseVariable name -> gamma `lookupType` name
    TypeOf exp -> typeOfExpression $ tInG exp
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
    Let var val body -> tInG $ Application (BaseExpression (Lambda var body)) val
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
    val@(BaseExpression _) -> val
    BaseVariable name -> gamma `lookupVariable` name
    -- Negation cases
    Negative (BaseExpression element) ->
        let (kind, size, num) = case element of
                GroupElement s n -> (GroupElement, s, n)
                FieldElement s n -> (FieldElement, s, n)
                _ -> undefined
        in kind size $ (-num) `mod` size
    Negative exp -> Negative $ eInG exp
    -- Inverse cases
    Inverse (BaseExpression (FieldElement size val)) ->
        BaseExpression $ maybe undefined (FieldElement size) (val `modInv` size)
    Inverse (BaseExpression _) -> undefined
    Inverse exp -> Inverse $ eInG exp
    -- Addition cases
    Addition (BaseExpression valA) (BaseExpression valB) ->
        let (kind, size, numA, numB) = case (valA, valB) of
                (GroupElement s a, GroupElement _ b) -> (GroupElement, s, a, b)
                (FieldElement s a, FieldElement _ b) -> (FieldElement, s, a, b)
                _ -> undefined
        in kind size $ (numA + numB) `mod` size
    Addition exp1@(BaseExpression _) exp2 -> Addition exp1 (eInG exp2)
    Addition exp1 exp2 -> Addition (eInG exp1) exp2
    -- Multiplicaiton cases
    Multiplicaiton (BaseExpression valA) (BaseExpression valB) ->
        let (kind, size, numA, numB = case (valA, valB) of
                (FieldElement s a, FieldElement _ b) -> (FieldElement, s, a, b)
                _ -> undefined
        in kind size $ (numA * numB) `mod` size
    Multiplication exp1@(BaseExpression _) exp2 -> Multiplication exp1 (eInG exp2)
    Multiplication exp1 exp2 -> Multiplication (eInG exp1) exp2
    -- AsElement cases
    -- AsNatural cases
    -- Application cases
    Application (BaseExpression (Lambda var body)) arg -> evalExp ((var, arg):gamma) body
    Application (BaseExpression _) _ -> undefined
    Application funcExp arg -> Application (eInG funcExp) arg
    -- Let cases (Let is just sugar!)
    Let var val body -> Application (BaseExpression (Lambda var body)) val
    -- Conditional cases
    Conditional (BaseExpression (Element 2 b)) expT expF -> eInG $ if 1 == b then expT else expF
    Conditional (BaseExpression _) _ _ -> undefined
    Conditional predicate expT expF -> Conditional (eInG predicate) expT expF
    where eInG = evalExp gamma

evaluateWith :: EvalContext -> Expression -> Expression
evaluateWith _ (BaseExpression val) = val
evaluateWith gamma exp = evaluateWith gamma $ evalExp gamma exp
evaluate = evaluateWith []




testExp = Application func five
          where t = Field 11
                x = BaseVariable "x"
                five = BaseExpression $ Element 11 5
                func = BaseExpression $ Lambda (Variable t "x") $ Addition x five

main :: IO ()
main = do putStrLn "Hello, Haskell!"
          print testExp
          print $ typeOf [] testExp
          print $ evalExp [] testExp
          print $ evaluate testExp




