module LLVM (Program(..), Function(..), Statement(..), Type(..), Operand, OpCode(..), integerOperand, typeOperand, registerOperand) where

import Text.Printf (printf)
import Data.List (intercalate)

data Program = Program
    { definitions :: [Function]
    -- statements needed to compute the value of the program and the register
    -- holding the final value
    , main :: ([Statement], Operand)
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
        , resultType :: Operand
        , opCode :: OpCode
        , left :: Operand
        , right :: Operand
        }

data Type
    = IntegerType

integerOperand :: Int -> Operand
integerOperand n = Operand (show n)

registerOperand :: Int -> Operand
registerOperand n = Operand ("%" ++ show n)

typeOperand :: Type -> Operand
typeOperand t = Operand (show t)

newtype Operand = Operand String
data OpCode = Add | Sub | Eq | Slt | Sgt | Sle | Sge

instance Show Program where
    show (Program _ (statements, register)) = printf
            "target triple = \"x86_64-pc-linux-gnu\"\n\
            \@fmt = private constant [4 x i8] c\"%%d\\0A\\00\"\n\
            \declare i32 @printf(i8*, ...)\n\
            \declare ptr @malloc(i64)\n\
            \\n\
            \define i32 @main() {\n\
            \    %s\n\
            \\n\
            \    %%fmt = getelementptr [4 x i8], [4 x i8]* @fmt, i64 0, i64 0\n\
            \    call i32 (i8*, ...) @printf(i8* %%fmt, i32 %s)\n\
            \    ret i32 0\n\
            \}\n\
            \"
            (intercalate "\n    " $ map show statements)
            (show register)

instance Show Statement where
    show operation = printf "%s = %s %s %s, %s"
            (show $ destination operation)
            (show $ opCode operation)
            (show $ resultType operation)
            (show $ left operation)
            (show $ right operation)



instance Show Type where
    show IntegerType = "i32"

instance Show Operand where
    show (Operand s) = s

instance Show OpCode where
    show Add = "add"
    show Sub = "sub"
    show Eq = "eq"
    show Slt = "slt"
    show Sgt = "sgt"
    show Sle = "sle"
    show Sge = "sge"
