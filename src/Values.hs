module Values where

data Value = Natural Integer
           | GroupElement Integer Integer  -- order, element
           | FieldElement Integer Integer  -- order, element
           | Orange Type Value  -- Left (right_type) value
           | Blue Type Value  -- Right (left_type) value
           | Pair Value Value
           | Lambda String Type Evaluation  -- var, from-type, to-val
    deriving (Eq, Show)

instance Expression Value where
  typeOf gamma e = Right $ case e of
    Natural _ -> Types.Natural
    GroupElement o _ -> Types.ModuloNumber o
    FieldElement o _ -> Types.ModuloPrime o
    Orange blueType orange -> Types.Choice (tInG orange) blueType
    Blue orangeType blue -> Types.Choice orangeType (tInG blue)
    Pair first second -> Types.Pair (tInG first) (tInG second)
    Lambda name from to ->
      let variable = Variable{typeOfVar=from,
                              nameOfVar=name,
                              valueOfVar=Nothing}
      in Types.Function from (typeOf (variable:gamma) to)
    where tInG = typeOf gamma

  step gamma e = ReducedValue $ case e of
    Lambda name from (Stepable c) ->
      let variable = Variable{typeOfVar=from,
                              nameOfVar=name,
                              valueOfVar=Nothing}
      in Lambda name from (step (variable:gamma) c)
    _ -> e



