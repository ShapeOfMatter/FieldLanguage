module Operators where

data Composition = TypeOf Evaluation
                 | Cast Evaluation Evaluation  -- as Choice () x, y
                 | Modulo Evaluation Evaluation
                 | Addition Evaluation Evaluation
                 | Negative Evaluation  -- a in {Group, Field} => a->a
                 | Inverse Evaluation
                 | Multiplication Evaluation Evaluation
                 | Unify Evaluation Evaluation Evaluation  -- (o->) (b->) (Choice o b)
                 | First Evaluation
                 | Second Evaluation
                 | Application Evaluation Evaluation  -- function, value
                 | Let Variable Evaluation  -- name type val exp
                 | Conditional Evaluation Evaluation Evaluation  -- pred true false
                 deriving (Eq, Show)

instance Expression Composition where
  typeOf gamma e = case e of
    TypeOf exp -> let typeOfExp = case typeOf gamma exp of
                                    Left c -> Stepable c
                                    Right t -> ReducedType t
                      typeOfTypeOfExp = typeOf gamma typeOfExp
                  in typeOfTypeOfExp
    Cast asType v -> fmap (Choice Unit) case asType of
      ReducedValue asValue -> undefined
      ReducedType t -> Right t
      Stepable c -> tInG $ Cast 
    where tInG = typeOf gamma




