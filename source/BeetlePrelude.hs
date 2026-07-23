module BeetlePrelude (prelude) where

import AST
import Data.Map (fromList, empty)
import Text.Megaparsec (SourcePos(..), initialPos)

prelude :: SourceExpression -> SourceExpression
prelude program =
    TypeAssignment "String" (SumType $ fromList [("Build", TupleType [IntegerType, UserType "String"]), ("Nil", RecordType empty)])
    program
    preludePosition

preludePosition :: SourcePos
preludePosition = initialPos "prelude"
