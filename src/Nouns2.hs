module Nouns where

data Expression = Type
    | TypeOf Expression
    | Cast Expression Expression
    | Modulo Expression Expression
    -- natural numbers, operations on naturals, parent
    | Natural Integer
    | Addition Expression Expression
    | Naturals
    -- group elements, operations, parents
    | GroupElement Integer Integer  -- group-size, number
    | Negative Expression  -- a in {Group, Field} => a->a
    | Group Integer  -- size/order
    | GroupClass
    -- field elements, operations, parents
    | FieldElement Integer Integer  -- field-size, number
    | Inverse Expression
    | Multiplication Expression Expression
    | Field Integer  -- size
    | FieldClass
    -- sum type
    | Orange Expression Expression  -- Left (right_type) value
    | Blue Expression Expression  -- Right (left_type) value
    | Unify Expression Expression Expression  -- (o->) (b->) (Choice o b)
    | Choice Expression Expression  -- Choice type type
    -- product
    | Pair Expression Expression
    | First Expression
    | Second Expression
    -- no parent type?
    -- functions
    | Lambda String Expression Expression  -- var-name, from-type, to-val
    | Function Expression Expression  -- from-type, to-type
    | Application Expression Expression  -- function, value
    | Let String Expression Expression Expression  -- name type val exp
    -- if
    | Conditional Expression Expression Expression  -- pred true false
    -- varibles
    | VariableReference String  -- does this work for Eq?
    deriving (Eq, Show)




