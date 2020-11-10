module Types where

data Type = Type
          | Natural
          | ModuloNumber Integer
          | Group
          | ModuloPrime Integer
          | Field
          | Choice Type Type  -- orange, blue
          | Pair Type Type
          | Function Type Type  -- From, To
          | Unit
          deriving (Eq, Show)

instance Expression Type where
  typeOf gamma e = Right $ case e of
    Type -> Type
    Natural -> Type
    ModuloNumber _ -> Group
    Group -> Type
    ModuloPrime _ -> Field
    Field -> Type
    Choice _ _ -> Type
    Pair _ _ -> Type
    Function _ _ -> Type
    Unit -> Type

  step gamma e = ReducedType e




