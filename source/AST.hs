{-# OPTIONS_GHC -Wincomplete-patterns #-}

module AST (Expression(..), Type(..), typeOf) where
import Data.Map

data Type = BooleanType | IntegerType | AliasedType String | TupleType [Type] | StructType (Map String Type) | FunctionType Type Type deriving (Show, Eq)

data Expression a
    = Boolean Bool
    | Integer Int
    | Tuple [Expression a] a
    | Struct (Map String (Expression a)) a
    | Variable String a
    | StructAccess String (Expression a) a
    | If (Expression a) (Expression a) (Expression a) a
    | Function Type Type String (Expression a)
    | Application (Expression a) (Expression a) a
    | Let String (Expression a) (Expression a) a
    | TypeLet String Type (Expression a) a
    | TupleDestructuring [String] (Expression a) (Expression a) a -- the list of destructured names, the tuple and the ensuing expression
    deriving Show

typeOf :: Expression Type -> Type
typeOf (Boolean _) = BooleanType
typeOf (Integer _) = IntegerType
typeOf (Tuple _ t) = t
typeOf (Struct _ t) = t
typeOf (Variable _ t) = t
typeOf (StructAccess _ _ t) = t
typeOf (TupleDestructuring _ _ _ t) = t
typeOf (If _ _ _ t) = t
typeOf (Function argumentType returnType _ _) = FunctionType argumentType returnType
typeOf (Application _ _ t) = t
typeOf (Let _ _ _ t) = t
typeOf (TypeLet _ _ _ t) = t
