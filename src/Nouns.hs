module Nouns where

data Variable = Variable {typeOfVar :: Value,
                          nameOfVar :: String}
                deriving (Eq, Show)

data Value = GroupElement Integer Integer  -- group-size, number
           | GroupType Integer  -- size
           | GroupClass
           | FieldElement Integer Integer  -- field-size, number
           | FieldType Integer  -- size
           | FieldClass
           | Lambda Variable Expression
           | Function Value Value
           | Type
             deriving (Eq, Show)

Natural = GroupClass

data Expression = BaseValue Value      -- v
                | BaseVariable String  -- x  -- not Variable?
                | GetTypeOf Expression    -- v->v
                | Negative Expression  -- a in {Group, Field} => a->a
                | Addition Expression Expression
                | Inverse Expression
                | Multiplication Expression Expression
                | AsElement Expression Expression
                | AsNatural Expression
                -- | AsPrime Expression
                | Application Expression Expression  -- function, value
                | Let Variable Expression Expression  -- Let var=exp in body
                | Conditional Expression Expression Expression  -- predicate, true-path, false-path
                  deriving (Show)




