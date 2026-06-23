module AST (Type(..), Expression(..), SourceExpression, TypedExpression, getPosition, getType) where

import Data.Map (Map)
import Text.Megaparsec (SourcePos)

data Type = IntegerType | BooleanType | UserType String | TupleType [Type] | RecordType (Map String Type) | FunctionType Type Type
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
    | TypeDeclaration
        { typeName :: String
        , aliasedType :: Type
        , body :: Expression a
        , annotation :: a
        }
    | Assignment
        { variableName :: String
        , variableValue :: Expression a
        , body :: Expression a
        , annotation :: a
        }
    | TupleDestructuring
        { destructuredNames :: [String]
        , tuple :: Expression a
        , body :: Expression a
        , annotation :: a
        }
    deriving Show

type SourceExpression = Expression SourcePos
getPosition :: SourceExpression -> SourcePos
getPosition others = annotation others

type TypedExpression = Expression Type
getType :: TypedExpression -> Type
getType others = annotation others
