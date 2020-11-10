module Evaluation where

class Expression e where
  typeOf :: Context -> e -> Either Composition Type
  step :: Context -> e -> Evaluation
  isMinimal :: Context -> e -> Bool

data Variable = Variable {typeOfVar :: Either Composition Type,
                          nameOfVar :: String
                          valueOfVar :: Maybe Evaluation}
                deriving (Eq, Show)

type Context = [Variable]

lookup :: Context -> String -> Variable
lookup (v:gamma) n = if n == (nameOfVar v)
                     then v
                     else lookup gamma n
lookup [] _ = undefined  -- Gotta represent failure somehow?

data Reference = Reference String

instance Reference Expression where
  typeOf gamma (Reference name) = typeOfVar (lookup gamma name)
  step gamma r@(Reference name) = case valueOfVar $ lookup gamma name of
                                  Just e -> e
                                  Nothing ->  r
  isMinimal gamma r@(Reference name) = case gamma of
                                         v:_ | name == nameOfVar v -> False
                                         _:g -> isMinimal g r
                                         [] -> True

data Evaluation = ReducedValue Value
                | ReducedType Type
                | Stepable Composition

instance Expression Evaluation where
  typeOf gamma e = case e of
    ReducedValue v -> typeOf v
    ReducedType t -> typeOf t
    Stepable c -> typeOf c

  step gamma e = case e of
    Stepable c -> step gamma c
    _ -> e


