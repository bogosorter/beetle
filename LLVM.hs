{-# OPTIONS_GHC -Wincomplete-patterns #-}

{-
This module provides a representation and export utilities for LLVM functions.
It is by no means representative of the whole LLVM IR, only of the subset used
here. It also doesn't ensure that all representable programs are correct.

TODO: Check for boolean format string output
-}

module LLVM where

import qualified AST

newtype Program = Program [TopLevelStatement]
instance Show Program where
    show (Program lines) = unlines (map show lines)

data TopLevelStatement
    = TargetTriple
    | FormatString
    | PrintfDeclaration
    | GlobalVariableDeclaration Type GlobalVar
    | Function Type GlobalVar (Maybe (Type, LocalVar)) [Statement] -- return type, name, argument type, name
instance Show TopLevelStatement where
    show TargetTriple = "target triple = \"x86_64-pc-linux-gnu\""
    show FormatString = "@fmt = private constant [4 x i8] c\"%d\\0A\\00\""
    show PrintfDeclaration = "declare i32 @printf(i8*, ...)"
    show (GlobalVariableDeclaration variableType variable) = show variable ++ " = global " ++ show variableType ++ " 0"
    show (Function returnType function argument body) =
        "define " ++ show returnType ++ " " ++ show function ++ "(" ++ buildArguments argument ++ ") {\n"
            ++ unlines (map show body) ++
        "}"
        where buildArguments Nothing = ""
              buildArguments (Just (argumentType, argument)) = show argumentType ++ " " ++ show argument

data Statement
    = LocalAssign Type Register Operation Operand Operand
    | Load Type Register GlobalVar
    | Store Type Register GlobalVar
    | Branch LocalVar Label Label
    | Jump Label
    | Phi Register Type Operand Label Operand Label
    | LabelStatement Label
    | Call Type Register GlobalVar Type LocalVar -- register to store and its type, function, argument type and name
    | FormatStringPointer
    | PrintfCall Type LocalVar
    | Return Type Operand
instance Show Statement where
    show (LocalAssign variableType register operation left right) =
        "    " ++ show register ++ " = " ++ show operation ++ " " ++ show variableType ++ " " ++ show left ++ ", " ++ show right
    show (Load loadType local global) =
        "    " ++ show local ++ " = load " ++ show loadType ++ ", ptr " ++ show global
    show (Store storeType local global) =
        "    store " ++ show storeType ++ " " ++ show local ++ ", ptr " ++ show global
    show (Branch variable left right) =
        "    br i1 " ++ show variable ++ ", label %" ++ show left ++ ", label %" ++ show right
    show (Jump label) =
        "    br label %" ++ show label
    show (Phi register resultType left leftLabel right rightLabel) =
        "    " ++ show register ++ "= phi " ++ show resultType ++ " [" ++ show left ++ ", %" ++ show leftLabel ++ "], [" ++ show right ++ ", %" ++ show rightLabel ++ "]"
    show (LabelStatement label) =
        show label ++ ":"
    show (Call returnType register function argumentType argument) =
        "    " ++ show register ++ " = call " ++ show returnType ++ " " ++ show function ++ "(" ++ show argumentType ++ " " ++ show argument ++ ")"
    show FormatStringPointer =
        "    %fmt = getelementptr [4 x i8], [4 x i8]* @fmt, i64 0, i64 0"
    show (PrintfCall variableType variable) =
        "    call i32 (i8*, ...) @printf(i8* %fmt, " ++ show variableType ++ " " ++ show variable ++ ")"
    show (Return returnType operand) =
        "    ret " ++ show returnType ++ " " ++ show operand

data Type = Boolean | Integer
instance Show Type where
    show Boolean = "i1"
    show Integer = "i32"

data Operation = Add | Sub | Eq
instance Show Operation where
    show Add = "add"
    show Sub = "sub"
    show Eq = "icmp eq"

data Operand = Literal Int | LocalOperand LocalVar
instance Show Operand where
    show (Literal value) = show value
    show (LocalOperand variable) = show variable

newtype GlobalVar = GlobalVar String
instance Show GlobalVar where
    show (GlobalVar name) = "@" ++ name

data LocalVar = ArgumentVar String | RegisterVar Register
instance Show LocalVar where
    show (ArgumentVar name) = "%" ++ name
    show (RegisterVar register) = show register

newtype Register = Register Int
instance Show Register where
    show (Register number) = "%" ++ show number

newtype Label = Label String
instance Show Label where
    show (Label label) = label

convertType :: AST.Type -> Type
convertType AST.BooleanType = Boolean
convertType AST.IntegerType = Integer
convertType (AST.FunctionType _ _) = error "llvm doesn't support function types"
