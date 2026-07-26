module Closures (Program(..), Type(..), Function(..), Expression(..), getType) where

data Program = Program
    { functions :: [Function]
    , main :: Expression
    }
    deriving Show

data Type
    = BooleanType
    | IntegerType
    | CharacterType
    | TupleType [Type]
    | SumType
    | ClosureType Type Type
    | GenericType
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
    | Character
        { asciiValue :: Int
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
        { value :: Expression
        , index :: Int
        , t :: Type
        }
    | Lowering
        { value :: Expression
        , t :: Type
        }
    | TypeAssertion
        { scrutinee :: Expression
        , index :: Int
        }
    | If
        { condition :: Expression
        , left :: Expression
        , right :: Expression
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
getType (Character {}) = CharacterType
getType (TypeAssertion {}) = BooleanType
getType expression = t expression
