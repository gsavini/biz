module Biz.Primitive ( primitives
                     , ioPrimitives
                     , writeProc
                     ) where

import Biz.Data
import System.IO
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError, runExceptT, catchError)
import Text.ParserCombinators.Parsec.Pos

primitives :: [(String, [BizVal] -> ErrorInfo -> ThrowsError BizVal)]
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

unpackNum :: BizVal -> ErrorInfo -> ThrowsError Double
unpackNum (Number n _) _ = return n
unpackNum notNum ei = throwError $ TypeMismatch "number" notNum ei

unpackText :: BizVal -> ErrorInfo -> ThrowsError String
unpackText (String s _) _ = return s
unpackText notString ei = throwError $ TypeMismatch "string" notString ei

unpackBool :: BizVal -> ErrorInfo -> ThrowsError Bool
unpackBool (Bool b _) _ = return b
unpackBool notBool ei = throwError $ TypeMismatch "boolean" notBool ei

unpackDoppio :: BizVal -> ErrorInfo -> ThrowsError [BizVal]
unpackDoppio (Doppio list _) _ = return list
unpackDoppio notDoppio ei = throwError $ TypeMismatch "doppio" notDoppio ei

throwNumArgs :: Integer -> [BizVal] -> ErrorInfo -> ThrowsError BizVal
throwNumArgs expected args ei = throwError $ NumArgs expected (toInteger . length $ args) ei

numBinop :: (Double -> Double -> Double) -> [BizVal] -> ErrorInfo -> ThrowsError BizVal
numBinop op args ei
  | length args /= 2 = throwNumArgs 2 args ei
  | otherwise = do
      res <- op <$> unpackNum (args !! 0) ei <*> unpackNum (args !! 1) ei
      return $ Number res pos

boolBinop :: (BizVal -> ErrorInfo -> ThrowsError a) -> (a -> a -> Bool) -> [BizVal] -> ErrorInfo -> ThrowsError BizVal
boolBinop unpacker op args ei
  | length args /= 2 = throwNumArgs 2 args ei
  | otherwise = do
      res <- op <$> unpacker (args !! 0) ei <*> unpacker (args !! 1) ei
      return $ Bool res pos

numBoolBinop  = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool

equal :: [BizVal] -> ErrorInfo -> ThrowsError BizVal
equal args ei
  | length args /= 2 = throwNumArgs 2 args ei
  | otherwise = return $ Bool ((args !! 0) == (args !! 1)) pos

notEqual :: [BizVal] -> ErrorInfo -> ThrowsError BizVal
notEqual args ei = do
  boolVal <- equal args ei
  case boolVal of
    Bool v _-> return $ Bool (not v) pos
    _ -> return $ Bool True pos

xor :: Bool -> Bool -> Bool
xor b1 b2 = b1 /= b2
             
xand :: Bool -> Bool -> Bool
xand b1 b2 = b1 == b2

peliNel :: [BizVal] -> ErrorInfo -> ThrowsError BizVal
peliNel args ei
  | length args /= 1 = throwNumArgs 1 args ei
  | otherwise = let bv = args !! 1
                in case bv of
                     CrepaBizError (CustomError _ (Just value) _) _-> return value
                     CrepaBizError (CustomError _ Nothing ei) _ -> throwError $ CrepaWithoutPeli bv ei
                     notCrepa -> throwError $ TypeMismatch "crepa" notCrepa ei

textOf :: [BizVal] -> ErrorInfo -> ThrowsError BizVal
textOf args ei
  | length args /= 1 = throwNumArgs 1 args ei
  | otherwise = let bv = args !! 0
                in case bv of
                     String s _ -> return bv
                     bv -> return $ String (show bv) (sourcePos bv)

appendText :: [BizVal] -> ErrorInfo -> ThrowsError BizVal
appendText args ei
  | length args /= 2 = throwNumArgs 2 args ei
  | otherwise = do
      s1 <- unpackText (args !! 0) ei
      s2 <- unpackText (args !! 1) ei
      return $ String (s1 ++ s2) (sourcePos $ args !! 1)

beep :: [BizVal] -> ErrorInfo -> ThrowsError BizVal
beep args ei
  | length args /= 1 = throwNumArgs 1 args ei
  | otherwise = do
      list <- unpackDoppio (args !! 0) ei
      return $ Doppio (tail list) pos

du :: [BizVal] -> ErrorInfo -> ThrowsError BizVal
du args ei
  | length args /= 2 = throwNumArgs 1 args ei
  | otherwise = do
       list <- unpackDoppio (args !! 1) ei
       return $ Doppio ((args !! 0) : list) pos

ru :: [BizVal] -> ErrorInfo -> ThrowsError BizVal
ru args ei
  | length args /= 2 = throwNumArgs 1 args ei
  | otherwise = do
      list <- unpackDoppio (args !! 1) ei
      return $ Doppio (list ++ [args !! 0]) pos

ioPrimitives :: [(String, [BizVal] -> ErrorInfo -> IOThrowsError BizVal)]
ioPrimitives = [("echoes", writeProc)
               ]
               
--makePort :: IOMode -> [BizVal] -> IOThrowsError BizVal
--makePort mode [String filename Nothing] = liftM $ Port (liftIO $ openFile filename mode) Nothing

writeProc :: [BizVal] -> ErrorInfo -> IOThrowsError BizVal
writeProc [obj] ei = writeProc [obj, Port stdout pos] ei
writeProc [obj, Port port _] ei = liftIO $ hPrint port obj >> (return $ Bool True pos)

--readContents :: [BizVal] -> IOThrowsError BizVal
--readContents [String filename Nothing] = liftM $ String (liftIO $ readFile filename) Nothing

pos :: SourcePos
pos = newPos "" 0 0