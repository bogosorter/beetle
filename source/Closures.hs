module Closures (Program(..), Type(..), FunctionDefinition(..), Expression(..), getType) where

data Program = Program
    { definitions :: [FunctionDefinition]
    , main :: Expression
    }

data Type = BooleanType | IntegerType | TupleType [Type] | ClosuredType Type Type

-- While not strictly necessary, we add name as a parameter to ensure that the
-- function is recognized in the produced LLVM Code
data FunctionDefinition
    = FunctionDefinition
        { functionName :: String
        , argumentType :: Type
        , returnType :: Type
        , closureType :: [Type]
        , functionBody :: Expression
        }
    | BuiltInFunction
        { functionName :: String
        , argumentType :: Type
        , returnType :: Type
        }

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
        { definition :: FunctionDefinition
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
    | If
        { condition :: Expression
        , left :: Expression
        , right :: Expression
        , t :: Type
        }
    | Application
        { function :: Expression
        , argument :: Expression
        , t :: Type
        }
    | Let
        { name :: String
        , value :: Expression
        , body :: Expression
        , t :: Type
        }

getType :: Expression -> Type
getType (Integer {}) = IntegerType
getType (Boolean {}) = BooleanType
getType expression = t expression
