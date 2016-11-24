module LispVal where


import System.IO hiding (try)
import Data.IORef
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)


-- Lisp data types --
data LispVal = Atom String
	     | List [LispVal]
	     | DottedList [LispVal] LispVal
	     | Number Integer
	     | String String
	     | Bool Bool
	     | Character Char
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
	     | Func {params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env}
	     | IOFunc ([LispVal] -> IOThrowsError LispVal)
	     | Port Handle


--
-- Deprecated Control.Monad.Error needed this but not Except package:
--
--instance Show LispError where
--     show EmptyString = "An error has occured"
--     show (O
--     noMsg = Default "An error has occured"
--     strMsg = Default

showVal :: LispVal -> String
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"
showVal (PrimitiveFunc _)      = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
	 "(lambda (" ++ unwords (map show args) ++ (case varargs of 
							Nothing -> ""
							Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _)	       = "<IO port>"
showVal (IOFunc _) 	       = "<IO primitive>"



-- convert list to string --
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


-- Environment --
-- map Strings to LispVals --
type Env = IORef [(String, IORef LispVal)]


{-- Error Handling --}
type ThrowsError = Either LispError

-- Use IO monad to handle layered errors/exceptions --
-- ExceptT is the monad transformer --
type IOThrowsError = ExceptT LispError IO


-- Show instantiations for Error and Values
instance Show LispError where show = showError

instance Show LispVal where show = showVal

    
-- Error types  --
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

trapError action = catchError action (return . show)


showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args: found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr


extractValue :: ThrowsError a -> a
extractValue (Right val) = val

