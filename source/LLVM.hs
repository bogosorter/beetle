{-# OPTIONS_GHC -Wincomplete-patterns #-}

{-
This module provides a representation for LLVM IR statements. It is by no means
representative of the whole LLVM IR, only of the subset used here.
-}

module LLVM (Program, TopLevelStatement(..), Statement(..)) where

import Data.List (intercalate)

newtype Program = Program [TopLevelStatement]

data TopLevelStatement
    = Function
        { fnReturnType :: Type
        , fnName :: String
        , fnArgumentType :: Type
        , fnBody :: [Statement]
        }
    | TypeDeclaration
        { tdName :: String
        , tdType :: Type
        }

data Statement
    = BinaryOperation
        { opType :: Type
        , opDest :: String
        , opOperation :: Operation
        , opLeftArg :: String
        , opRightArg :: String
        }
    | GetElementPointer
        { gepDest :: String
        , gepElementType :: String
        , gepBasePtr :: String
        , gepOffset :: String
        , gepIndex :: Maybe String
        }
    | BitCast
        { bcDest :: String
        , bcTargetType :: Type
        , bcSrc :: String
        , bcSrcType :: Type
        }
    | PtrToInt
        { ptiDest :: String
        , ptiSrc :: String
        }
    | Malloc
        { mallocDest :: String
        , mallocSize :: String
        }
    | Load
        { loadDest :: String
        , loadType :: Type
        , loadSrc :: String
        }
    | Store
        { storeType :: Type
        , storeSrc :: String
        , storeDest :: String
        }
    | Branch
        { brCond :: String
        , brTrueLabel :: String
        , brFalseLabel :: String
        }
    | Jump
        { jumpLabel :: String
        }
    | Phi
        { phiDest :: String
        , phiType :: Type
        , phiValA :: String
        , phiLabelA :: String
        , phiValB :: String
        , phiLabelB :: String
        }
    | LabelStatement
        { labelName :: String
        }
    | Call
        { callReturnType :: Type
        , callDest :: String
        , callFunc :: String
        , callEnv :: String
        , callArgType :: Type
        , callArg :: String
        }
    | FormatStringPointer -- loads the format string onto a register
    | PrintfCall
        { printfArgType :: Type
        , printfArg :: String
        }
    | Return
        { retType :: Type
        , retVal :: String
        }

data Type = Boolean | Integer | Tuple [Type] | Pointer
data Operation = Add | Sub | Eq | Slt | Sgt | Sle | Sge


instance Show Program where
    show (Program lines) = unlines (map show lines)

instance Show TopLevelStatement where
    show TargetTriple = "target triple = \"x86_64-pc-linux-gnu\""
    show FormatString = "@fmt = private constant [4 x i8] c\"%d\\0A\\00\""
    show PrintfDeclaration = "declare i32 @printf(i8*, ...)"
    show MallocDeclaration = "declare ptr @malloc(i64)"
    show (Function returnType function argument body) =
        "define " ++ show returnType ++ " " ++ show function ++ "(" ++ buildArguments argument ++ ") {\n"
            ++ unlines (map show body) ++
        "}"
        where buildArguments Nothing = ""
              buildArguments (Just (argumentType, argument)) = "ptr %env, " ++ show argumentType ++ " " ++ show argument
    show (TypeDeclaration variable t) = show variable ++ " = type " ++ show t

instance Show Statement where
    show (BinaryOperation variableType register operation left right) =
        "    " ++ show register ++ " = " ++ show operation ++ " " ++ show variableType ++ " " ++ show left ++ ", " ++ show right
    show (GetElementPointer result elementType base offset index) =
        let indexSuffix = case index of
                Just i -> ", i32 " ++ show index
                Nothing -> ""
        in "    " ++ show result ++ " = getelementptr " ++ show elementType ++ ", ptr " ++ show base ++ ", i32 " ++ show offset ++ indexSuffix
    show (BitCast result elementType element resultType) =
        "    " ++ show result ++ " = bitcast " ++ show elementType ++ " " ++ show element ++ " to " ++ show resultType
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
