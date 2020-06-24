module Main where

import Text.ParserCombinators.Parsec (Parser, parse)
import System.IO ( hFlush
                 , stdout
                 , hPutStrLn
                 , stderr
                 )
import System.Environment (getArgs)
import Control.Monad (liftM)
import Control.Monad.Except (throwError, runExceptT, catchError)

import Biz.Parser (topLevel, expr)
import Biz.Data
import Biz.Eval (eval, bindVars, nullEnv, nullStashEnvs, evalLoad)
import Biz.Primitive

import Data.IORef
import Control.Monad.IO.Class ( liftIO)

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne args

runRepl :: IO ()
runRepl = do
  env <- primitiveBindings
  stashEnvs <- nullStashEnvs
  until_ (== "quit") (readPrompt "biz > ") (evalAndPrint env stashEnvs)

runOne :: [String] -> IO ()
runOne args = do
  pb <- primitiveBindings
  env <- bindVars pb [("args", LangList (map LangString $ drop 1 args))] "__lang__"
  stashEnvs <- nullStashEnvs
  runIOThrows env (show <$> evalLoad env stashEnvs (head args)) >>= hPutStrLn stderr

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "biz" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

evalString :: Env -> StashEnvs -> String -> IO String
evalString env stashEnvs expr = runIOThrows env $ fmap show $ liftThrows (readOrThrow topLevel expr) >>= eval env stashEnvs

evalAndPrint :: Env -> StashEnvs -> String -> IO ()
evalAndPrint env stashEnvs expr = evalString env stashEnvs expr >>= putStrLn -- >> printEnv env >> printKq stashEnvs

runIOThrows :: Env -> IOThrowsError String -> IO String
runIOThrows env action = extractValue <$> runExceptT (trapError env action)

makeFunc varargs env params body = return $ Func (map show params) body env

primitiveBindings :: IO Env
primitiveBindings = do
  ne <- nullEnv
  bindVars ne (map (makeFunc $ IOFunc) ioPrimitives
               ++ map (makeFunc $ PrimitiveFunc) primitives
               ++ [ ("__part__", LangString "__unknown__")
                  , ("__filename__", LangString "__unknown__")
                  , ("__mainpath__", LangString "__unknown__")
                  , ("__fullname__", LangString "__unknown__")
                  , ("__stacktrace__", LangList [])
                  ]) "__lang__"
  where makeFunc constructor (var, func) = (var, constructor func)

trapError :: Env -> IOThrowsError String -> IOThrowsError String
trapError env action = catchError action
  (\bizError -> return $ show bizError)

printEnv :: Env -> IO ()
printEnv env = do
  e <- readIORef env
  mapM_ print e
  where print x = do
          bv <- readIORef $ mValue x
          attr <- readIORef $ mAttributes x
          putStrLn (mName x ++ " " ++ show bv ++ " " ++ show attr ++ " " ++ mPart x)

printKq :: StashEnvs -> IO ()
printKq kq = do
  k <- readIORef kq
  mapM_ print k
  where print (name, x) = putStrLn name >> printEnv x