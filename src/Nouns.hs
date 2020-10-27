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
                | Addition Expression Expression  -- a in {Group, Natural, Field} => a->a->a
                | Inverse Expression   -- Field x -> Field x
                | Multiplication Expression Expression  -- Field x -> Field x
                | Modulo Expression Expression  -- a in {Natural, Prime}, b in {Group, Natural, Field} => a->b->a
                | Application Expression Expression  -- function, value
                | Let Variable Expression Expression  -- Let var=exp in body
                | Conditional Expression Expression Expression  -- predicate, true-path, false-path
                  deriving (Show)




