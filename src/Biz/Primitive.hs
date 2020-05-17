module Biz.Primitive ( primitives
                     , ioPrimitives
                     , writeProc
                     ) where

import Biz.Data
import System.IO
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError, runExceptT, catchError)

primitives :: [(String, [BizVal] -> ThrowsError BizVal)]
primitives = [ ("+", numBinop (+))
             , ("-", numBinop (-))
             , ("*", numBinop (*))
             , ("/", numBinop (/))
             , ("=", equal)
             , ("/=", notEqual)
             , ("<", numBoolBinop (<))
             , ("<=", numBoolBinop (<=))
             , (">", numBoolBinop (>))
             , (">=", numBoolBinop (>=))
             , ("and", boolBoolBinop (&&))
             , ("or", boolBoolBinop (||))
             , ("xand", boolBoolBinop xand)
             , ("xor", boolBoolBinop xor)
             , ("with", appendText)
             , ("beep", beep)
             , ("textOf", textOf)
             , ("peliNel", peliNel)
             , ("du", du)
             , ("ru", ru)
             ]

unpackNum :: BizVal -> ThrowsError Double
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackText :: BizVal -> ThrowsError String
unpackText (String s) = return s
unpackText notString  = throwError $ TypeMismatch "string" notString

unpackBool :: BizVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackDoppio :: BizVal -> ThrowsError [BizVal]
unpackDoppio (Doppio list) = return list
unpackDoppio notDoppio = throwError $ TypeMismatch "doppio" notDoppio

throwNumArgs :: Integer -> [BizVal] -> ThrowsError BizVal
throwNumArgs expected args = throwError $ NumArgs expected (toInteger . length $ args)

numBinop :: (Double -> Double -> Double) -> [BizVal] -> ThrowsError BizVal
numBinop op args
  | length args /= 2 = throwNumArgs 2 args
  | otherwise = do
      res <- op <$> unpackNum (args !! 0) <*> unpackNum (args !! 1)
      return $ Number res

boolBinop :: (BizVal -> ThrowsError a) -> (a -> a -> Bool) -> [BizVal] -> ThrowsError BizVal
boolBinop unpacker op args
  | length args /= 2 = throwNumArgs 2 args
  | otherwise = do
      res <- op <$> unpacker (args !! 0) <*> unpacker (args !! 1)
      return $ Bool res

numBoolBinop  = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool

equal :: [BizVal] -> ThrowsError BizVal
equal args
  | length args /= 2 = throwNumArgs 2 args
  | otherwise = return $ Bool $ (args !! 0) == (args !! 1)

notEqual :: [BizVal] -> ThrowsError BizVal
notEqual args = do
  boolVal <- equal args
  case boolVal of
    Bool v -> return $ Bool (not v)
    _ -> return $ Bool True

xor :: Bool -> Bool -> Bool
xor b1 b2 = b1 /= b2
             
xand :: Bool -> Bool -> Bool
xand b1 b2 = b1 == b2

peliNel :: [BizVal] -> ThrowsError BizVal
peliNel args
  | length args /= 1 = throwNumArgs 1 args
  | otherwise = let bv = args !! 1
                in case bv of
                     Crepa (CustomError _ (Just a)) -> return a
                     Crepa (CustomError _ Nothing) -> throwError $ CrepaWithoutPeli bv
                     notCrepa -> throwError $ TypeMismatch "crepa" notCrepa

textOf :: [BizVal] -> ThrowsError BizVal
textOf args
  | length args /= 1 = throwNumArgs 1 args
  | otherwise = let bv = args !! 0
                in case bv of
                     String s -> return bv
                     _ -> return $ String $ show bv

appendText :: [BizVal] -> ThrowsError BizVal
appendText args
  | length args /= 2 = throwNumArgs 2 args
  | otherwise = do
      s1 <- unpackText (args !! 0)
      s2 <- unpackText (args !! 1)
      return $ String (s1 ++ s2)

beep :: [BizVal] -> ThrowsError BizVal
beep args
  | length args /= 1 = throwNumArgs 1 args
  | otherwise = do
      list <- unpackDoppio $ args !! 0
      return . Doppio . tail $ list

du :: [BizVal] -> ThrowsError BizVal
du args
  | length args /= 2 = throwNumArgs 1 args
  | otherwise = do
       list <- (unpackDoppio $ args !! 1)
       return $ Doppio $ (args !! 0) : list

ru :: [BizVal] -> ThrowsError BizVal
ru args
  | length args /= 2 = throwNumArgs 1 args
  | otherwise = do
      list <- (unpackDoppio $ args !! 1)
      return $ Doppio $ list ++ [args !! 0]

ioPrimitives :: [(String, [BizVal] -> IOThrowsError BizVal)]
ioPrimitives = [("echoes", writeProc)
               ]
               
makePort :: IOMode -> [BizVal] -> IOThrowsError BizVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

writeProc :: [BizVal] -> IOThrowsError BizVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [BizVal] -> IOThrowsError BizVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
