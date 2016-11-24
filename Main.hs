module Main where

import Parser
import LispVal
import Evaluator
import Control.Monad
import System.Environment
import System.IO hiding (try)



main :: IO ()
main = do args <- getArgs
	  if null args then runRepl else runOne $ args


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine


evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env


evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do result <- prompt
                               if pred result then return ()
                               else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do 
	env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
	(runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) 	 
	>>=hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "exit") (readPrompt ">>> ") . evalAndPrint

