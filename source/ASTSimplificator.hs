module ASTSimplificator (simplify) where

import AST

import qualified Data.Map (map, empty)

simplify :: TypedExpression -> TypedExpression
simplify expression = case expression of
    Boolean {} -> expression
    Integer {} -> expression
    Character {} -> expression
    Tuple members t -> Tuple (map simplify members) t
    Record members t -> Record (Data.Map.map simplify members) t
    EmptyList t -> Constructor "ListNil" (Record Data.Map.empty (RecordType Data.Map.empty)) t
    EmptyString t -> Constructor "ListNil" (Record Data.Map.empty (RecordType Data.Map.empty)) t
    Constructor constructor value t -> Constructor constructor (simplify value) t
    Function {} -> expression { body = simplify $ body expression }
    If condition left right t -> If (simplify condition) (simplify left) (simplify right) t
    IfLet scrutinee constructor introducedVariable introducedType left right t ->
        Match (simplify scrutinee) [(constructor, introducedVariable, introducedType, simplify left)] (Just $ simplify right) t
    Match scrutinee branches Nothing t ->
        let simplifyBranch (name, introducedVariable, introducedType, body) = (name, introducedVariable, introducedType, simplify body)
        in Match (simplify scrutinee) (map simplifyBranch branches) Nothing t
    Match {} -> error "match expressions shouldn't have default branches at this point"
    Unwrap name introducedType scrutinee constructor body t ->
        Match (simplify scrutinee) [(constructor, name, introducedType, body)] Nothing t

    -- Since the LLVM does not provide a true modulo operator, it is complicated
    -- (ehem, simplified) to only use the remainder
    -- a mod b = (a rem b + b) rem b
    Application (Application (Variable "mod" _) left _) right _ ->
        let abRem = Application (Application (Variable "rem" IntegerType) left IntegerType) right IntegerType
            bPlus = Application (Application (Variable "+" IntegerType) abRem IntegerType) right IntegerType
            bRem = Application (Application (Variable "rem" IntegerType) bPlus IntegerType) right IntegerType
        in simplify $ bRem

    -- and is simplified to an if expression
    Application (Application (Variable "and" _) left _) right _ ->
        simplify $ If left right (Boolean False BooleanType) BooleanType

    -- or is simplified to an if expression
    Application (Application (Variable "or" _) left _) right _ ->
        simplify $ If left (Boolean True BooleanType) right BooleanType

    -- Other applications are left untouched
    Application function argument t -> Application (simplify function) (simplify argument) t

    RecordMember {} -> expression { record = simplify $ record expression }
    Variable {} -> expression
    TypeAssignment assignedName assignedType body t -> TypeAssignment assignedName assignedType (simplify body) t
    Assignment name value body t -> Assignment name (simplify value) (simplify body) t
    TupleDestructuring names tuple body t -> TupleDestructuring names (simplify tuple) (simplify body) t
