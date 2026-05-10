{-# OPTIONS_GHC -Wincomplete-patterns #-}

module AST (Expression(..), Type(..), typeOf) where

data Type = BooleanType | IntegerType | FunctionType Type Type deriving (Show, Eq)

data Expression a
    = Boolean Bool
    | Integer Int
    | Variable String a
    | If (Expression a) (Expression a) (Expression a) a
    | Function Type Type String (Expression a)
    | Application (Expression a) (Expression a) a
    | Let String (Expression a) (Expression a) a
    deriving Show

typeOf :: Expression Type -> Type
typeOf (Boolean _) = BooleanType
typeOf (Integer _) = IntegerType
typeOf (Variable _ t) = t
typeOf (If _ _ _ t) = t
typeOf (Function argumentType returnType _ _) = FunctionType argumentType returnType
typeOf (Application _ _ t) = t
typeOf (Let _ _ _ t) = t
