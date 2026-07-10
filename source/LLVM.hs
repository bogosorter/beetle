{-# OPTIONS_GHC -Wno-name-shadowing #-}

module LLVM (Program(..), Function(..), Statement(..), Type(..), Operand, Label(..), OpCode(..), integerOperand, typeOperand, registerOperand, variableOperand) where

import Text.Printf (printf)
import Data.List (intercalate)

data Type
    = IntegerType
    | BooleanType

data Program = Program
    { definitions :: [Function]
    -- statements needed to compute the value of the program and the register
    -- holding the final value, as well as its value
    , main :: ([Statement], Operand, Type)
    }

data Function = Function
    { returnType :: Operand
    , name :: Operand
    , environmentType :: Operand
    , argument :: Operand
    , argumentType :: Operand
    , body :: [Statement]
    }

data Statement
    = Operation
        { destination :: Operand
        , resultType :: Type
        , opCode :: OpCode
        , leftOperand :: Operand
        , rightOperand :: Operand
        }
    | Bitcast
        { destination :: Operand
        , destinationType :: Type
        , source :: Operand
        , sourceType :: Type
        }
    | Label
        { label :: Label
        }
    | Jump
        { label :: Label
        }
    | Branch
        { condition :: Operand
        , leftLabel :: Label
        , rightLabel :: Label
        }
    | Phi
        { destination :: Operand
        , destinationType :: Type
        , leftOperand :: Operand
        , leftLabel :: Label
        , rightOperand :: Operand
        , rightLabel :: Label
        }

newtype Operand = Operand String
newtype Label = MakeLabel String
data OpCode = Add | Sub | Eq | Slt | Sgt | Sle | Sge

integerOperand :: Int -> Operand
integerOperand n = Operand (show n)

registerOperand :: Int -> Operand
registerOperand n = Operand ("%" ++ show n)

variableOperand :: String -> Operand
variableOperand s = Operand ("%" ++ s)

typeOperand :: Type -> Operand
typeOperand t = Operand (show t)

instance Show Program where
    show (Program _ (statements, register, resultType)) = printf
            "target triple = \"x86_64-pc-linux-gnu\"\n\
            \@fmt = private constant [4 x i8] c\"%%d\\0A\\00\"\n\
            \declare i32 @printf(i8*, ...)\n\
            \declare ptr @malloc(i64)\n\
            \\n\
            \define i32 @main() {\n\
            \    %s\n\
            \\n\
            \    %%fmt = getelementptr [4 x i8], [4 x i8]* @fmt, i64 0, i64 0\n\
            \    call i32 (i8*, ...) @printf(i8* %%fmt, %s %s)\n\
            \    ret i32 0\n\
            \}\n\
            \"
            (intercalate "\n    " $ map show statements)
            (show resultType)
            (show register)

instance Show Statement where
    show statement = case statement of
        Operation {} ->
            printf "%s = %s %s %s, %s"
            (show $ destination statement)
            (show $ opCode statement)
            (show $ resultType statement)
            (show $ leftOperand statement)
            (show $ rightOperand statement)

        Bitcast {} -> printf "%s = bitcast %s %s to %s"
            (show $ destination statement)
            (show $ sourceType statement)
            (show $ source statement)
            (show $ destinationType statement)

        Label label ->
            printf "%s:" (show label)

        Jump label ->
            printf "br label %%%s" (show label)

        Branch {} ->
            printf "br i1 %s, label %%%s, label %%%s"
            (show $ condition statement)
            (show $ leftLabel statement)
            (show $ rightLabel statement)

        Phi {} ->
            printf "%s = phi %s [%s, %%%s], [%s, %%%s]"
            (show $ destination statement)
            (show $ destinationType statement)
            (show $ leftOperand statement)
            (show $ leftLabel statement)
            (show $ rightOperand statement)
            (show $ rightLabel statement)


instance Show Type where
    show IntegerType = "i32"
    show BooleanType = "i1"

instance Show Operand where
    show (Operand s) = s

instance Show Label where
    show (MakeLabel s) = s

instance Show OpCode where
    show Add = "add"
    show Sub = "sub"
    show Eq = "icmp eq"
    show Slt = "icmp slt"
    show Sgt = "icmp sgt"
    show Sle = "icmp sle"
    show Sge = "icmp sge"
