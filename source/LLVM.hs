module LLVM (Program(..), Function(..), Statement(..), Type(..), Operand, Label(..), OpCode(..), integerOperand, typeOperand, registerOperand, variableOperand, globalOperand, LLVM.null) where

import Text.Printf (printf)
import Data.List (intercalate)

data Type
    = VoidType
    | IntegerType
    | BooleanType
    | TupleType [Type]
    | PointerType

data Program = Program
    { definitions :: [Function]
    -- statements needed to compute the value of the program and the register
    -- holding the final value, as well as its value
    , main :: ([Statement], Operand, Type)
    }

data Function = Function
    { name :: Operand
    , argumentType :: Type
    , returnType :: Type
    , body :: [Statement]
    }

data Statement
    = BinaryOperation
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
    | GetElementPointer
        { destination :: Operand
        , t :: Type
        , base :: Operand
        , index :: Operand
        , member :: Operand
        }
    | GetElementPointerSimple
        { destination :: Operand
        , t :: Type
        , base :: Operand
        , index :: Operand
        }
    | PointerToInt
        { destination :: Operand
        , source :: Operand
        }
    | Malloc
        { destination :: Operand
        , size :: Operand
        }
    | Load
        { destination :: Operand
        , destinationType :: Type
        , address :: Operand
        }
    | Store
        { source :: Operand
        , sourceType :: Type
        , address :: Operand
        }
    | Call
        { destination :: Operand
        , destinationType :: Type
        , function :: Operand
        , environment :: Operand
        , argument :: Operand
        , callArgumentType :: Type
        }
    | Return
        { source :: Operand
        , sourceType :: Type
        }
    | Comment
        { content :: String
        }
    | EmptyLine

newtype Operand = Operand String
newtype Label = MakeLabel String
data OpCode = Mul | Div | Rem | Add | Sub | Eq | Slt | Sgt | Sle | Sge

integerOperand :: Int -> Operand
integerOperand n = Operand (show n)

registerOperand :: Int -> Operand
registerOperand n = Operand ("%" ++ show n)

variableOperand :: String -> Operand
variableOperand s = Operand ("%" ++ s)

typeOperand :: Type -> Operand
typeOperand t = Operand (show t)

globalOperand :: String -> Operand
globalOperand s = Operand ("@" ++ s)

null :: Operand
null = Operand "null"

instance Show Program where
    show (Program functions (statements, register, resultType)) = printf
            "target triple = \"x86_64-pc-linux-gnu\"\n\
            \@fmt = private constant [4 x i8] c\"%%d\\0A\\00\"\n\
            \declare i32 @printf(i8*, ...)\n\
            \declare ptr @malloc(i64)\n\
            \\n\
            \%s\
            \\n\
            \\n\
            \define i32 @main() {\n\
            \    %s\n\
            \\n\
            \    %%_fmt = getelementptr [4 x i8], [4 x i8]* @fmt, i64 0, i64 0\n\
            \    call i32 (i8*, ...) @printf(i8* %%_fmt, %s %s)\n\
            \    ret i32 0\n\
            \}\n\
            \"
            (intercalate "\n\n" $ map show functions)
            (intercalate "\n    " $ map show statements)
            (show resultType)
            (show register)

instance Show Function where
    show function = printf
        "define %s %s(ptr %%_env, %s %%_argument) {\n\
        \    %s\n\
        \}"
        (show $ returnType function)
        (show $ name function)
        (show $ argumentType function)
        (intercalate "\n    " $ map show (body function))


instance Show Statement where
    show statement = case statement of
        BinaryOperation {} ->
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

        GetElementPointer {} ->
            printf "%s = getelementptr %s, ptr %s, i32 %s, i32 %s"
            (show $ destination statement)
            (show $ t statement)
            (show $ base statement)
            (show $ index statement)
            (show $ member statement)

        GetElementPointerSimple {} ->
            printf "%s = getelementptr %s, ptr %s, i32 %s"
            (show $ destination statement)
            (show $ t statement)
            (show $ base statement)
            (show $ index statement)

        PointerToInt {} ->
            printf "%s = ptrtoint ptr %s to i64"
            (show $ destination statement)
            (show $ source statement)

        Malloc {} ->
            printf "%s = call ptr @malloc(i64 %s)"
            (show $ destination statement)
            (show $ size statement)

        Load {} ->
            printf "%s = load %s, ptr %s"
            (show $ destination statement)
            (show $ destinationType statement)
            (show $ address statement)

        Store {} ->
            printf "store %s %s, ptr %s"
            (show $ sourceType statement)
            (show $ source statement)
            (show $ address statement)

        Call {} ->
            printf "%s = call %s %s(ptr %s, %s %s)"
            (show $ destination statement)
            (show $ destinationType statement)
            (show $ function statement)
            (show $ environment statement)
            (show $ callArgumentType statement)
            (show $ argument statement)

        Return {} ->
            printf "ret %s %s"
            (show $ sourceType statement)
            (show $ source statement)

        Comment {} ->
            printf "; %s"
            (content statement)

        EmptyLine {} ->
            ""


instance Show Type where
    show VoidType = "void"
    show IntegerType = "i32"
    show BooleanType = "i1"
    show (TupleType ts) = "{" ++ (intercalate ", " $ map show ts) ++ "}"
    show PointerType = "ptr"

instance Show Operand where
    show (Operand s) = s

instance Show Label where
    show (MakeLabel s) = s

instance Show OpCode where
    show Mul = "mul"
    show Div = "sdiv"
    show Rem = "srem"
    show Add = "add"
    show Sub = "sub"
    show Eq = "icmp eq"
    show Slt = "icmp slt"
    show Sgt = "icmp sgt"
    show Sle = "icmp sle"
    show Sge = "icmp sge"
