module Error (Error(..), ErrorType(..), ErrorMessage, errorAtUnknown) where

import Position(Position, unknownPosition)

type ErrorMessage = String

data Error = Error {
               errorType     :: ErrorType,
               errorPosition :: Position,
               errorMessage  :: ErrorMessage
             }
  deriving (Eq, Show)

data ErrorType =
    InternalError
  | LexerErrorInvalidCharacter
  | LexerErrorMalformedName
  | LexerErrorLayout
  | LexerErrorUnclosedComment
  | LexerErrorInvalidEscapeSequence
  | LexerErrorInvalidCharacterConstant
  | LexerErrorUnterminatedString
  | ReaderErrorCyclicDependencies
  | ReaderErrorModuleNameMismatch
  | ReaderErrorMissingFile
  | ParseErrorExpectedToken
  | ParseErrorExpectedModule
  | ParseErrorExpectedId
  | ParseErrorExpectedInt
  | ParseErrorTypeHasNoHead
  | ParseErrorDataHasNoHead
  | ParseErrorEquationHasNoHead
  | ParseErrorExpectedNameForSignature
  | ParseErrorPrematureEndOfExpression
  | ParseErrorOperatorPartUsedAsVariable
  | ParseErrorExpectedExpression
  | ModuleSystemError
  | DesugaringError
  deriving (Eq, Show)

errorAtUnknown :: ErrorType -> String -> Error
errorAtUnknown typ msg = Error typ unknownPosition msg

