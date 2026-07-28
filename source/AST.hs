module AST (Type(..), Expression(..), SourceExpression, TypedExpression, getPosition, getType) where

import qualified Data.Map as Map
import qualified Data.List as List
import Text.Megaparsec (SourcePos)

data Type
    = BooleanType
    | IntegerType
    | CharacterType
    | TupleType [Type]
    | RecordType (Map.Map String Type)
    | SumType [String] (Map.Map String Type)
    | FunctionType Type Type
    | UserType String [Type]
    | TypeVariable String
    | TypeVariableInstance Int
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
    | Character
        { asciiValue :: Int
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
    | EmptyList
        { annotation :: a
        }
    | EmptyString
        { annotation :: a
        }
    | Constructor
        { constructor :: String
        , value :: Expression a
        , annotation :: a
        }
    -- Lowerings are introduced by the Type-Checker, and should thus not be in
    -- the AST. Perhaps this needs some refactoring.
    | Lowering
        { value :: Expression a
        , constructor :: String
        , annotation :: a
        }
    | TypeAssertion
        { scrutinee :: Expression a
        , constructor :: String
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
    | TypeAssignment
        { assignedName :: String
        , assignedType :: Type
        , body :: Expression a
        , annotation :: a
        }
    | Assignment
        { assignedName :: String
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
    show IntegerType = "Integer"
    show BooleanType = "Boolean"
    show CharacterType = "Character"
    show (TupleType memberTypes) = "(" ++ List.intercalate ", " (map show memberTypes) ++ ")"
    show (RecordType memberTypes) = "{" ++ List.intercalate ", " (map showRecordMemberType (Map.toList memberTypes)) ++ "}"
    show (SumType _ constructors) = List.intercalate " | " [constructor | (constructor, _) <- Map.toList constructors]
    -- We check for a function type on the left side and add parentheses, since
    -- function types are normally right-associative
    show (FunctionType argumentType returnType) = case argumentType of
        FunctionType {} -> "(" ++ show argumentType ++ ") -> " ++ show returnType
        _ -> show argumentType ++ " -> " ++ show returnType
    show (UserType name parameters) = name ++ "<" ++ List.intercalate ", " (map show parameters) ++ ">"
    show (TypeVariable name) = name
    show (TypeVariableInstance n) = "t" ++ show n

showRecordMemberType :: (String, Type) -> String
showRecordMemberType (name, t) = name ++ ": " ++ show t
