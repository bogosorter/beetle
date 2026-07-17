module Closures (Program(..), Type(..), Function(..), Expression(..), getType) where

data Program = Program
    { functions :: [Function]
    , main :: Expression
    }
    deriving Show

data Type
    = BooleanType
    | IntegerType
    | TupleType [Type]
    | ClosureType Type Type
    | SumType [Type]
    | ConstructorType Type
    deriving Show

-- While not strictly necessary, we add name as a parameter to ensure that the
-- function is recognized in the produced LLVM Code
data Function = Function
    { functionName :: String
    , argumentType :: Type
    , returnType :: Type
    , environmentType :: [Type]
    , functionBody :: Expression
    }
    deriving Show

data Expression
    = Boolean
        { booleanValue :: Bool
        }
    | Integer
        { integerValue :: Int
        }
    | Tuple
        { members :: [Expression]
        , t :: Type
        }
    | Closure
        { definition :: Function
        , environment :: [Expression]
        , t :: Type
        }
    | Argument
        { t :: Type
        }
    | Local
        { name :: String
        , t :: Type
        }
    | Captured
        { index :: Int -- The position of the variable in the closure
        , t :: Type
        }
    | TupleMember
        { index :: Int
        , tuple :: Expression
        , t :: Type
        }
    | Constructor
        { index :: Int
        , value :: Expression
        , t :: Type
        }
    | If
        { condition :: Expression
        , left :: Expression
        , right :: Expression
        , t :: Type
        }
    | Case
        { scrutinee :: Expression
        , branches :: [(String, Type, Expression)]
        , t :: Type
        }
    | Application
        { closure :: Expression
        , argument :: Expression
        , t :: Type
        }
    | Let
        { name :: String
        , value :: Expression
        , body :: Expression
        , t :: Type
        }
    | BuiltInFunction
        { name :: String
        , t :: Type
        }
    deriving Show

getType :: Expression -> Type
getType (Integer {}) = IntegerType
getType (Boolean {}) = BooleanType
getType expression = t expression
