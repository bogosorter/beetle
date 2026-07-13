module ASTSimplificator (simplify) where

import AST

import qualified Data.Map (map)

simplify :: TypedExpression -> TypedExpression
simplify expression = case expression of
    Boolean {} -> expression
    Integer {} -> expression
    Tuple members t -> Tuple (map simplify members) t
    Record members t -> Record (Data.Map.map simplify members) t
    Function {} -> expression { body = simplify $ body expression }
    If condition left right t -> If (simplify condition) (simplify left) (simplify right) t
    Application function argument t -> Application (simplify function) (simplify argument) t
    RecordMember {} -> expression { record = simplify $ record expression }
    Variable {} -> expression
    Assignment name value body t -> Assignment name (simplify value) (simplify body) t
    TupleDestructuring names tuple body t -> TupleDestructuring names (simplify tuple) (simplify body) t

    TypeDeclaration {} -> error "type checked AST should not have type declarations"
