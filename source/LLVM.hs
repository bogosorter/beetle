module LLVM (Program(..), Function(..), Statement(..), Type(..), Operand, Label(..), OpCode(..), integerOperand, booleanOperand, characterOperand, typeOperand, registerOperand, variableOperand, globalOperand, LLVM.null) where

import Text.Printf (printf)
import Data.List (intercalate)
import Data.Char (isAlphaNum)

data Type
    = VoidType
    | IntegerType
    | BooleanType
    | CharacterType
    | TupleType [Type]
    | PointerType
    | BoxedType
    deriving Eq

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
    | Switch
        { source :: Operand
        , defaultLabel :: Label
        , destinations :: [(Operand, Label)]
        }
    | Phi
        { destination :: Operand
        , destinationType :: Type
        , sources :: [(Operand, Label)]
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
    | IntToPointer
        { destination :: Operand
        , source :: Operand
        }
    | ZeroExtend
        { destination :: Operand
        , destinationType :: Type
        , source :: Operand
        , sourceType :: Type
        }
    | Truncate
        { destination :: Operand
        , destinationType :: Type
        , source :: Operand
        , sourceType :: Type
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
data OpCode = Mul | Div | Rem | Add | Sub | Eq | Slt | Sgt | Sle | Sge | Xor

integerOperand :: Int -> Operand
integerOperand n = Operand (show n)

booleanOperand :: Bool -> Operand
booleanOperand False = Operand (show (0 :: Integer))
booleanOperand True = Operand (show (1 :: Integer))

characterOperand :: Int -> Operand
characterOperand c = Operand (show c)

registerOperand :: Int -> Operand
registerOperand n = Operand ("%" ++ show n)

variableOperand :: String -> Operand
variableOperand s = Operand ("%" ++ escape s)

typeOperand :: Type -> Operand
typeOperand t = Operand (show t)

globalOperand :: String -> Operand
globalOperand s = Operand ("@" ++ escape s)

null :: Operand
null = Operand "null"

instance Show Program where
    show (Program functions (statements, resultRegister, resultType)) = printf
            "target triple = \"x86_64-pc-linux-gnu\"\n\
            \declare void @print_boolean(i8)\n\
            \declare void @print_integer(i32)\n\
            \declare void @print_character(i8)\n\
            \declare void @print_string(ptr)\n\
            \declare ptr @malloc(i64)\n\
            \\n\
            \%s\
            \\n\
            \\n\
            \define i32 @main() {\n\
            \    %s\n\
            \\n\
            \%s\n\
            \    ret i32 0\n\
            \}\n\
            \"
            (intercalate "\n\n" $ map show functions)
            (intercalate "\n    " $ map show statements)
            (showPrint resultType resultRegister)

        where showPrint :: Type -> Operand -> String
              showPrint resultType resultRegister
                | resultType == BooleanType = printf
                    "    ; the result is zero-extended to prevent print from reading garbage\n\
                    \    ; values when printing booleans\n\
                    \    %%_extended_result = zext i1 %s to i8\n\
                    \    call void @print_boolean(i8 %%_extended_result)\n"
                    (show resultRegister)
                | resultType == IntegerType = printf
                    "    call void @print_integer(i32 %s)\n"
                    (show resultRegister)
                | resultType == CharacterType = printf
                    "    call void @print_character(i8 %s)\n"
                    (show resultRegister)
                | resultType == PointerType = printf
                    "    call void @print_string(ptr %s)\n"
                    (show resultRegister)
                | otherwise = error "got a result type that is not of type string, boolean, character or pointer"

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

        Switch {} ->
            let showDestination :: (Operand, Label) -> String
                showDestination (operand, label) = printf "i32 %s, label %%%s" (show operand) (show label)
            in printf "switch i32 %s, label %%%s [\n        %s\n    ]"
            (show $ source statement)
            (show $ defaultLabel statement)
            (intercalate "\n        " $ map showDestination (destinations statement))

        Phi {} ->
            let showSource :: (Operand, Label) -> String
                showSource (operand, label) = printf "[%s, %%%s]" (show operand) (show label)
            in printf "%s = phi %s %s"
            (show $ destination statement)
            (show $ destinationType statement)
            (intercalate ", " $ map showSource (sources statement))

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

        IntToPointer {} ->
            printf "%s = inttoptr i64 %s to ptr"
            (show $ destination statement)
            (show $ source statement)

        ZeroExtend {} ->
            printf "%s = zext %s %s to %s"
            (show $ destination statement)
            (show $ sourceType statement)
            (show $ source statement)
            (show $ destinationType statement)

        Truncate {} ->
            printf "%s = trunc %s %s to %s"
            (show $ destination statement)
            (show $ sourceType statement)
            (show $ source statement)
            (show $ destinationType statement)

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
    show CharacterType = "i8"
    show (TupleType ts) = "{" ++ (intercalate ", " $ map show ts) ++ "}"
    show PointerType = "ptr"
    show BoxedType = "i64"

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
    show Xor = "xor"

-- Utils

-- An approximation of LLVM's escaping mechanism
escape :: String -> String
escape s
    | not (all isAllowed s) = "\"" ++ s ++ "\""
    | otherwise = s
    where isAllowed c = c `elem` ['_', '.', '$'] || isAlphaNum c
