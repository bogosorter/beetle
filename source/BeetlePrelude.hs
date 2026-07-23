module BeetlePrelude (prelude) where

import AST
import Data.Map (fromList, empty)
import Text.Megaparsec (SourcePos(..), initialPos)

prelude :: SourceExpression -> SourceExpression
prelude program =
    TypeAssignment "String" (SumType $ fromList [("StringConstructor", TupleType [CharacterType, UserType "String"]), ("StringNil", RecordType empty)])
    program
    preludePosition

preludePosition :: SourcePos
preludePosition = initialPos "prelude"
