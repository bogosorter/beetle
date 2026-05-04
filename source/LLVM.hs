{-# OPTIONS_GHC -Wincomplete-patterns #-}

{-
This module provides a representation and export utilities for LLVM functions.
It is by no means representative of the whole LLVM IR, only of the subset used
here. It also doesn't ensure that all representable programs are correct.

TODO: Check for boolean format string output
-}

module LLVM where

import qualified Closures
import Data.List (intercalate)

newtype Program = Program [TopLevelStatement]
instance Show Program where
    show (Program lines) = unlines (map show lines)

data TopLevelStatement
    = TargetTriple
    | FormatString
    | PrintfDeclaration
    | MallocDeclaration
    | GlobalVariableDeclaration Type GlobalVar
    | Function Type GlobalVar (Maybe (Type, LocalVar)) [Statement] -- return type, name, argument type, name
    | TypeDeclaration TypeVar Type

data Statement
    = LocalAssign Type Register Operation Operand Operand
    | GetElementPointer Operand TypeVar Operand Operand Operand
    | GetElementPointer2 Operand TypeVar Operand Operand
    | PtrToInt Operand Operand
    | Malloc Operand Operand
    | Load Type Operand Operand
    | Store Type Operand Operand
    | Branch LocalVar Label Label
    | Jump Label
    | Phi Register Type Operand Label Operand Label
    | LabelStatement Label
    | Call Type Operand Operand Operand Type LocalVar -- register to store and its type, function, environment, argument type and name
    | FormatStringPointer
    | PrintfCall Type LocalVar
    | Return Type Operand

data Type = Boolean | Integer | Tuple [Type] | Pointer
data Operation = Add | Sub | Eq | Slt | Sgt | Sle | Sge
data Operand = Literal Int | LocalOperand LocalVar | GlobalOperand GlobalVar | NullPtr
newtype GlobalVar = GlobalVar String
newtype TypeVar = TypeVar String
data LocalVar = ArgumentVar | RegisterVar Register | LocalVar String
newtype Register = Register Int
newtype Label = Label String


convertType :: Closures.Type -> Type
convertType Closures.BooleanType = Boolean
convertType Closures.IntegerType = Integer
convertType (Closures.ClosureType _ _) = Pointer

instance Show TopLevelStatement where
    show TargetTriple = "target triple = \"x86_64-pc-linux-gnu\""
    show FormatString = "@fmt = private constant [4 x i8] c\"%d\\0A\\00\""
    show PrintfDeclaration = "declare i32 @printf(i8*, ...)"
    show MallocDeclaration = "declare ptr @malloc(i64)"
    show (GlobalVariableDeclaration variableType variable) = show variable ++ " = global " ++ show variableType ++ " 0"
    show (Function returnType function argument body) =
        "define " ++ show returnType ++ " " ++ show function ++ "(" ++ buildArguments argument ++ ") {\n"
            ++ unlines (map show body) ++
        "}"
        where buildArguments Nothing = ""
              buildArguments (Just (argumentType, argument)) = "ptr %env, " ++ show argumentType ++ " " ++ show argument
    show (TypeDeclaration variable t) = show variable ++ " = type " ++ show t

instance Show Statement where
    show (LocalAssign variableType register operation left right) =
        "    " ++ show register ++ " = " ++ show operation ++ " " ++ show variableType ++ " " ++ show left ++ ", " ++ show right
    show (GetElementPointer result elementType base offset index) =
        "    " ++ show result ++ " = getelementptr " ++ show elementType ++ ", ptr " ++ show base ++ ", i32 " ++ show offset ++ ", i32 " ++ show index
    show (GetElementPointer2 result elementType base offset) =
        "    " ++ show result ++ " = getelementptr " ++ show elementType ++ ", ptr " ++ show base ++ ", i32 " ++ show offset
    show (PtrToInt destination pointer) = "    " ++ show destination ++ " = ptrtoint ptr " ++ show pointer ++ " to i64"
    show (Malloc destination size) = "    " ++ show destination ++ " = call ptr @malloc(i64 " ++ show size ++ ")"
    show (Load loadType local global) =
        "    " ++ show local ++ " = load " ++ show loadType ++ ", ptr " ++ show global
    show (Store storeType local global) =
        "    store " ++ show storeType ++ " " ++ show local ++ ", ptr " ++ show global
    show (Branch variable left right) =
        "    br i1 " ++ show variable ++ ", label %" ++ show left ++ ", label %" ++ show right
    show (Jump label) =
        "    br label %" ++ show label
    show (Phi register resultType left leftLabel right rightLabel) =
        "    " ++ show register ++ " = phi " ++ show resultType ++ " [" ++ show left ++ ", %" ++ show leftLabel ++ "], [" ++ show right ++ ", %" ++ show rightLabel ++ "]"
    show (LabelStatement label) =
        show label ++ ":"
    show (Call returnType register function environment argumentType argument) =
        "    " ++ show register ++ " = call " ++ show returnType ++ " " ++ show function ++ "(ptr " ++ show environment ++ ", " ++ show argumentType ++ " " ++ show argument ++ ")"
    show FormatStringPointer =
        "    %fmt = getelementptr [4 x i8], [4 x i8]* @fmt, i64 0, i64 0"
    show (PrintfCall variableType variable) =
        "    call i32 (i8*, ...) @printf(i8* %fmt, " ++ show variableType ++ " " ++ show variable ++ ")"
    show (Return returnType operand) =
        "    ret " ++ show returnType ++ " " ++ show operand

instance Show Type where
    show Boolean = "i1"
    show Integer = "i32"
    show (Tuple types) = "{ " ++ (intercalate ", " (map show types)) ++ " }"
    show Pointer = "ptr"

instance Show Operation where
    show Add = "add"
    show Sub = "sub"
    show Eq = "icmp eq"
    show Slt = "icmp slt"
    show Sgt = "icmp sgt"
    show Sle = "icmp sle"
    show Sge = "icmp sge"

instance Show Operand where
    show (Literal value) = show value
    show (LocalOperand variable) = show variable
    show (GlobalOperand operand) = show operand
    show NullPtr = "null"

instance Show GlobalVar where
    show (GlobalVar name) = "@" ++ name

instance Show TypeVar where
    show (TypeVar name) = "%" ++ name

instance Show LocalVar where
    show ArgumentVar = "%argument"
    show (RegisterVar register) = show register
    show (LocalVar name) = "%" ++ name

instance Show Register where
    show (Register number) = "%" ++ show number

instance Show Label where
    show (Label label) = label
