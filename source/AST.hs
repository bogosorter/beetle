module AST where

import Data.Map (Map)
import Text.Megaparsec (SourcePos)

data Type = IntegerType | BooleanType | TupleType [Type] | RecordType (Map String Type) | FunctionType Type Type
    deriving (Show, Eq)

data Expression a
    = Integer
        { integerValue :: Int
        , annotation :: a
        }
    | Boolean
        { booleanValue :: Bool
        , annotation :: a
        }
    | Tuple
        { tupleMembers :: [Expression a]
        , annotation :: a
        }
    | Record
        { recordMembers :: Map String (Expression a)
        , annotation :: a
        }
    | Function
        { argumentType :: Type
        , returnType :: Type
        , argumentName :: String
        , body :: Expression a
        , annotation :: a
        }
    | If
        { condition :: Expression a
        , left :: Expression a
        , right :: Expression a
        , annotation :: a
        }
    | Application
        { function :: Expression a
        , argument :: Expression a
        , annotation :: a
        }
    | Variable
        { variableName :: String
        , annotation :: a
        }
    | RecordMember
        { record :: Expression a
        , memberName :: String
        , annotation :: a
        }
    | TypeAlias
        { name :: String
        , aliasedType :: Type
        , annotation :: a
        }
    | Let
        { variableName :: String
        , variableValue :: Expression a
        , body :: Expression a
        , annotation :: a
        }
    | TupleDestructuring
        { tuple :: Expression a
        , destructuredNames :: [String]
        , body :: Expression a
        , annotation :: a
        }

getPosition :: Expression SourcePos -> SourcePos
getPosition others = annotation others

getType :: Expression Type -> Type
getType others = annotation others
