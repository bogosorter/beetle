{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AST (Type(..), Expression(..), SourceExpression, TypedExpression, getPosition, getType) where

import qualified Data.Map as Map
import qualified Data.List as List
import Text.Megaparsec (SourcePos)

data Type = BooleanType | IntegerType | UserType String | TupleType [Type] | RecordType (Map.Map String Type) | FunctionType Type Type
    deriving Eq

data Expression a
    = Boolean
        { booleanValue :: Bool
        , annotation :: a
        }
    | Integer
        { integerValue :: Int
        , annotation :: a
        }
    | Tuple
        { tupleMembers :: [Expression a]
        , annotation :: a
        }
    | Record
        { recordMembers :: Map.Map String (Expression a)
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

instance Show Type where
    show IntegerType = "integer"
    show BooleanType = "boolean"
    show (UserType name) = name
    show (TupleType memberTypes) = "(" ++ List.intercalate ", " (map show memberTypes) ++ ")"
    show (RecordType memberTypes) = "{" ++ List.intercalate ", " (map showRecordMemberType (Map.toList memberTypes)) ++ "}"
    -- We check for a function type on the left side and add parentheses, since
    -- function types are normally right-associative
    show (FunctionType argumentType returnType) = case argumentType of
        FunctionType {} -> "(" ++ show argumentType ++ ") -> " ++ show returnType
        _ -> show argumentType ++ " -> " ++ show returnType

showRecordMemberType :: (String, Type) -> String
showRecordMemberType (name, t) = name ++ ": " ++ show t
