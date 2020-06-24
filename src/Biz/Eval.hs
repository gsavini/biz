module Biz.Eval ( eval
                , bindVars
                , nullEnv
                , nullStashEnvs
                , evalLoad
                ) where

import Control.Monad (liftM, mapM)
import Control.Monad.Except (throwError, runExceptT, catchError)
import qualified Control.Exception as CE
import Control.Monad.IO.Class ( liftIO)
import System.IO
import Text.ParserCombinators.Parsec --(Parser, parse, endBy, spaces, sepBy, many1, many, sepEndBy, eof)
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Pos
import Text.Parsec.Pos
import Data.IORef
import Data.Either (fromRight)
import Data.List (isPrefixOf, findIndex, tails, intersperse, intercalate)
import qualified Data.Map as M
import Data.Maybe (isJust)
import System.Directory as SD (getCurrentDirectory, makeAbsolute)
import System.FilePath as FP

import Biz.Parser (expr, spaces1, topLevel, eeol)
import Biz.Data
import Biz.Helper

readExprList = readOrThrow $ (sepEndBy (spaces >> topLevel) (try $ many (char ' ' <|> char '\t') >> eeol))

eval :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
eval = eval'

eval' :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
eval' _ _ val@String {} = return val
eval' _ _ val@Number {} = return val
eval' _ _ val@Bool {} = return val
eval' env _ (Atom id sp) = getVar env id sp
eval' env stashEnvs val@(Var (Atom name _) value attributes _) = evalVar env stashEnvs val
eval' env stashEnvs val@If {} = evalIf env stashEnvs val
eval' env stashEnvs val@Seq {} = evalSeq env stashEnvs val 0
eval' env stashEnvs val@FuncCall {} = evalFuncCall env stashEnvs val
eval' env stashEnvs val@(Funcf (Atom name _) params body _) = evalFuncDef env stashEnvs val
eval' env stashEnvs val@HolHorse {} = evalHolHorse env stashEnvs val
eval' env stashEnvs val@ForLoop {} = evalForLoop env stashEnvs val
eval' env stashEnvs val@DoppioLoop {} = evalDoppioLoop env stashEnvs val
eval' env stashEnvs val@InfiniteLoop {} = evalInfiniteLoop env stashEnvs val
eval' env stashEnvs val@Arrivederci {} = return val
eval' env stashEnvs val@KingCrimson {} = return val
eval' env stashEnvs val@MoodyBlues {} = return val
eval' env stashEnvs val@Mista {} = undefined
eval' env stashEnvs val@Doppio {} = return val
eval' env stashEnvs val@Dururu {} = evalDururu env stashEnvs val
eval' env stashEnvs val@Rurudu {} = evalRurudu env stashEnvs val
eval' _ _ val@User {} = return val
eval' _ _ val@Stand {} = return val
eval' env stashEnvs val@Cry {} = evalCry env stashEnvs val
eval' env stashEnvs val@Arrow {} = evalArrow env stashEnvs val
eval' env stashEnvs val@Ability {} = evalAbility env stashEnvs val
eval' env stashEnvs val@SingleArrow {} = undefined
eval' env stashEnvs val@KillerQueen {} = evalKillerQueen env stashEnvs val
eval' env stashEnvs val@BitesTheDust {} = evalBitesTheDust env stashEnvs val
eval' env stashEnvs val@TheHand {} = evalTheHand env stashEnvs val
eval' env stashEnvs val@Part {} = evalPart env stashEnvs val
eval' env stashEnvs val@SkipPart {} = evalSkip env stashEnvs val
eval' env stashEnvs val@GirlsDontLikeGuys {} = evalGirlsDontLikeGuys env stashEnvs val
eval' env stashEnvs val@PorcaMiseria {} = evalPorcaMiseria env stashEnvs val
eval' env stashEnvs val@Cazzo {} = evalCazzo env stashEnvs val
eval' env stashEnvs val@Crepa {} = evalCrepa env stashEnvs val

evalVar :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalVar env stashEnvs (Var (Atom name _) value attributes sp) = do
  bv <- eval env stashEnvs value
  case bv of
    None -> throw env NoneIsUnbindable sp
    _ -> defineVar env name bv attributes

evalIf :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalIf env stashEnvs (If pred maybeLeft maybeRight maybeBoth sp) =
  do result <- eval env stashEnvs pred
     case result of
       Bool False _ -> case (maybeLeft, maybeBoth) of
                      (Nothing, Nothing) -> return None
                      (Just left, Nothing) -> eval env stashEnvs left
                      (Just left, Just both) -> do eval env stashEnvs left
                                                   eval env stashEnvs both
                      (Nothing, Just both) -> eval env stashEnvs both
       Bool True _ -> case (maybeRight, maybeBoth) of
                       (Nothing, Nothing) -> return None
                       (Just right, Nothing) -> eval env stashEnvs right
                       (Just right, Just both) -> do eval env stashEnvs right
                                                     eval env stashEnvs both
                       (Nothing, Just both) -> eval env stashEnvs both
       notBool -> throw env (TypeMismatch "boolean" notBool) (sourcePos pred)

evalSeq :: Env -> StashEnvs -> BizVal -> Integer -> IOThrowsError BizVal
evalSeq env stashEnvs (Seq [] _) _ = return None
evalSeq env stashEnvs (Seq [x] _) _ = do
  result <- eval env stashEnvs x
  case result of
    Arrivederci expr _ -> eval env stashEnvs expr
    KingCrimson sp -> eval env stashEnvs (KingCrimson sp)
    MoodyBlues _ _ -> return None
    _ -> return result
evalSeq env stashEnvs (Seq exps sp) n = do
  result <- eval env stashEnvs (exps !! fromInteger n)
  case result of
    Arrivederci expr _ -> eval env stashEnvs expr
    KingCrimson sp -> eval env stashEnvs (KingCrimson sp)
    MoodyBlues number _ -> moodyHelper env stashEnvs number n exps sp
    _ -> if fromInteger (n + 1) < length exps
         then evalSeq env stashEnvs (Seq exps sp) (n + 1)
         else return result
  where moodyHelper env stashEnvs number n exps sp
          | number == 0 = return None
          | number <= n = evalSeq env stashEnvs (Seq (delete (fromInteger n) exps) sp) (n - number)
          | otherwise = let newList = delete (fromInteger n) exps
                            newLength = (toInteger $ length newList)
                        in evalSeq env stashEnvs (Seq newList sp) $ newLength - 1 - (number `mod` newLength)

evalFuncDef :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalFuncDef env stashEnvs (Funcf (Atom name _) params body sp) = do
  let fun = Func (map (\(Atom n _) -> n) params) body env stashEnvs sp
  defineVar env name fun []

evalAbility :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalAbility env stashEnvs (Ability name value sp) = do
  evalVal <- eval env stashEnvs value
  return (Ability name evalVal sp)

evalCry :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalCry env stashEnvs (Cry [a@(Atom name _)] _) = eval env stashEnvs a
evalCry env stashEnvs (Cry [value] _) = return value
evalCry env stashEnvs (Cry (bizVal:(Atom a2 spA):xs) sp) = do
  v <- eval env stashEnvs bizVal
  case v of
    User uName stands _ ->
      case M.lookup a2 stands of
        Nothing -> throw env (FieldDoesntExist uName a2) spA
        Just stand -> evalCry env stashEnvs (Cry (stand:xs) sp)
    Stand sName abilities _ ->
      case M.lookup a2 abilities of
        Nothing -> throw env (FieldDoesntExist sName a2) spA
        Just ability -> evalCry env stashEnvs (Cry (ability:xs) sp)

evalArrow :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalArrow env stashEnvs (Arrow sp atom1 atom2) = do
  value1 <- eval env stashEnvs atom1
  value2 <- eval env stashEnvs atom2
  case (value1, value2) of
    (s@(Stand sName sAbilities _) , User userName stands sp) ->
      defineVar env userName (User userName (M.insertWith mergeStands sName s stands) sp) []
    (Ability aName aValue _, Stand standName abilities sp) -> return $ Stand standName (M.insert aName aValue abilities) sp
    _ -> throw env (WrongArrow value1 value2) sp

clearComments :: [String] -> [String]
clearComments [] = []
clearComments (x:xs) =
  let maybeIndex = findString "speedwagon" x
  in case maybeIndex of
    Just index -> take index x : clearComments xs
    Nothing -> x : clearComments xs
  where -- https://stackoverflow.com/a/48198347
    findString :: (Eq a) => [a] -> [a] -> Maybe Int
    findString search str = findIndex (isPrefixOf search) (tails str)

load :: Env -> String -> SourcePos -> IOThrowsError [BizVal]
load env part sp = do
  (LangString curDir) <- getVar env "__mainpath__" sp
  let filename = curDir </> replace '.' '/' part <.> ".bz"
  content <- liftIO (CE.try $ readFile filename :: IO (Either CE.SomeException String))
  case content of
    Left _ -> do let fn = curDir ++ replace '.' '/' part ++ ".biz"
                 content <- liftIO (CE.try $ readFile filename :: IO (Either CE.SomeException String))
                 case content of
                   Left _ -> throw env (PartDoesntExist part) sp
                   Right r -> readContent r
    Right r -> readContent r
  where readContent r = do let clear = unlines . clearComments . lines $ r
                           liftThrows . readExprList $ clear

evalLoad :: Env -> StashEnvs -> String -> IOThrowsError BizVal
evalLoad env stashEnvs filename = do
  setVar env "__mainpath__" (LangString (FP.takeDirectory filename)) []
  absolutePath <- liftIO $ SD.makeAbsolute filename
  setVar env "__fullname__" (LangString absolutePath) []
  let ext = takeExtension filename
    in
    if ext == ".bz" || ext == ".biz"
    then evalSkip env stashEnvs (SkipPart (FP.takeBaseName filename) Nothing (newPos "biz" 0 0))
    else throw env (WrongBizFile filename) (newPos "biz" 0 0)

evalDururu :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalDururu env stashEnvs (Dururu number moshi bv sp) = do
  result <- eval env stashEnvs bv
  case result of
    Doppio list sp -> case moshi of
                     Nothing -> return $ list !! fromInteger number
                     Just val -> return $ Doppio (replaceNth (fromInteger number) val list) sp
    notDoppio -> throw env (TypeMismatch "doppio" notDoppio) sp

evalRurudu :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalRurudu env stashEnvs (Rurudu number moshi bv sp) = do
  result <- eval env stashEnvs bv
  case result of
    Doppio list sp -> case moshi of
                     Nothing -> return $ reverse list !! fromInteger number
                     Just val -> return $ Doppio (reverse . replaceNth (fromInteger number) val $ reverse list) sp
    notDoppio -> throw env (TypeMismatch "doppio" notDoppio) sp

evalHolHorse :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalHolHorse env stashEnvs val@(HolHorse params body sp) =
  return $ Func params body env stashEnvs sp

evalFuncCall :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalFuncCall env stashEnvs (FuncCall name args sp) = do
  func <- eval env stashEnvs name
  argVals <- mapM (eval env stashEnvs) args
  apply env func argVals sp

apply :: Env -> BizVal -> [BizVal] -> SourcePos -> IOThrowsError BizVal
apply env (PrimitiveFunc func) args sp = do
  ei <- errorInfo env sp
  liftThrows $ func args ei
apply env (IOFunc func) args sp = do
  ei <- errorInfo env sp
  func args ei
apply env (Func params body closure stashEnvs _) args sp =
  if num params /= num args
  then throw env (NumArgs (num params) (num args)) sp
  else do
    (String part _) <- getPart closure sp
    prepareEnv closure
    env <- liftIO $ bindVars closure (zip params args) part
    result <- eval env stashEnvs body
    case result of
      Arrivederci expr _ -> eval env stashEnvs expr
      _ -> return result
  where num = toInteger . length

evalInfiniteLoop :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalInfiniteLoop env stashEnvs val@(InfiniteLoop body _) = do
  result <- eval env stashEnvs body
  case result of
    Arrivederci expr _ -> eval env stashEnvs expr
    _ -> evalInfiniteLoop env stashEnvs val

evalForLoop :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalForLoop env stashEnvs (ForLoop (Atom localVar spa) start inc predicate body sp) = do
  evalStart <- eval env stashEnvs start
  localEnv <- do
    (String part _) <- getPart env sp
    liftIO $ bindVars env [(localVar, evalStart)] part
  evalInc <- eval localEnv stashEnvs inc
  evalPred <- eval localEnv stashEnvs predicate
  case (evalStart, evalInc, evalPred) of
    (Number s _, Number i spI, Bool b _) ->
      case evalPred of
        Bool True _ -> do
          result <- eval localEnv stashEnvs body
          case result of
            Arrivederci expr _ -> eval localEnv stashEnvs expr
            _ -> evalForLoop localEnv stashEnvs (ForLoop (Atom localVar spa) (Number (s + i) spI) inc predicate body sp)
        _ -> return None

    (Number _ _, Number _ _, _) -> throw env (TypeMismatch "bool" evalPred) sp
    (Number _ _, _, _) -> throw env (TypeMismatch "number" evalInc) sp
    (_, _, _) -> throw env (TypeMismatch "number" evalStart) sp

evalDoppioLoop :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalDoppioLoop env stashEnvs loop@(DoppioLoop (Atom localVar spA) doppio body sp) = do
  bv <- eval env stashEnvs doppio
  case bv of
    Doppio [] _ -> return None
    Doppio (x:xs) spd -> do
      (String part _) <- getPart env sp
      localEnv <- liftIO $ bindVars env [(localVar, x)] part
      result <- eval localEnv stashEnvs body
      case result of
        (Arrivederci expr _) -> eval localEnv stashEnvs expr
        notArr -> if length xs == 0
        then return notArr
        else evalDoppioLoop env stashEnvs (DoppioLoop (Atom localVar spA) (Doppio xs spd) body sp)
    notDoppio -> throw env (TypeMismatch "doppio" notDoppio) sp

evalGirlsDontLikeGuys :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalGirlsDontLikeGuys env stashEnvs (GirlsDontLikeGuys expVal predicate items sp) = do
  evalPred <- eval env stashEnvs predicate
  case evalPred of
    PrimitiveFunc _ -> do
      val <- eval env stashEnvs expVal
      evalGirl' env stashEnvs evalPred val items
    Func _ _ _ _ _-> do
      val <- eval env stashEnvs expVal
      evalGirl' env stashEnvs evalPred val items
    notPred -> throw env (GirlsLikePredicates notPred) sp
      

  where evalGirl' :: Env -> StashEnvs -> BizVal -> BizVal -> [BizVal] -> IOThrowsError BizVal
        evalGirl' _ _ _ _ [] = return None
        evalGirl' env stashEnvs pred val ((GirlItem value effect sp):xs) =
          case value of
            Atom "steal" _ -> eval env stashEnvs effect
            bizVal -> do bv <- eval env stashEnvs bizVal
                         res <- apply env pred [val, bv] sp
                         case res of
                           Bool True _ -> eval env stashEnvs effect
                           Bool False _ -> evalGirl' env stashEnvs pred val xs
                           notBool -> throw env (GirlsLikePredicates notBool) sp


evalPorcaMiseria :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalPorcaMiseria env stashEnvs (PorcaMiseria origin bizVal sp) =
  catchError
  (eval env stashEnvs bizVal)
  (\bizError -> evalFuncCall env stashEnvs (FuncCall (Atom "whoWroteAllThis" sp) [String origin sp, CrepaBizError bizError sp] sp))

evalCazzo :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalCazzo env stashEnvs (Cazzo error _) = do
  e <- eval env stashEnvs error
  case e of
    CrepaBizError bizError _ -> throwError bizError
    notCrepa -> do
      ei <-  errorInfo env (sourcePos notCrepa)
      throwError $ CazzoWithoutCrepa notCrepa ei

evalCrepa :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalCrepa env stashEnvs (Crepa errorName maybeVal sp) = do
    ei <- errorInfo env sp
    return $ CrepaBizError (CustomError errorName maybeVal ei) sp

prepareEnv :: Env -> IOThrowsError Env
prepareEnv envRef = do
  env <- liftIO $ readIORef envRef
  constEnv envRef env

constEnv :: Env -> [Mut] -> IOThrowsError Env
constEnv env [] = return env
constEnv env (x:xs) = do
  va <- liftIO $ readIORef $ mAttributes x
  bizVal <- liftIO $ readIORef $ mValue x
  if Reliable `elem` va || NoDignity `elem` va
    then constEnv env xs
    else do setVarPart env (mName x) bizVal (Reliable : va) (mPart x)
            constEnv env xs

isStashEnvBound :: StashEnvs -> String -> IO Bool
isStashEnvBound envRef var = isJust . lookup var <$> readIORef envRef

evalKillerQueen :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalKillerQueen env stashEnvs (KillerQueen name sp) = do
  bound <- liftIO $ isStashEnvBound stashEnvs name
  if bound
    then do boundEnv <- getEnv env stashEnvs name sp
            clonedEnv <- liftIO $ cloneEnv env
            c <- liftIO $ readIORef clonedEnv
            liftIO $ writeIORef boundEnv c
            return None
    else liftIO $ do se <- readIORef stashEnvs
                     clonedEnv <- cloneEnv env
                     writeIORef stashEnvs ((name, clonedEnv) : se)
                     return None

cloneEnv :: Env -> IO Env
cloneEnv env = do
  e <- readIORef env
  n <- nullEnv
  cloneEnv' e n
  where cloneEnv' :: [Mut] -> Env -> IO Env
        cloneEnv' [] env = return env
        cloneEnv' (x:xs) envRef = do
          value <- readIORef $ mValue x
          valueRef <- newIORef value
          attrs <- readIORef $ mAttributes x
          attrsRef <- newIORef attrs
          env <- readIORef envRef
          writeIORef envRef (Mut (mName x) valueRef attrsRef (mPart x) : env)
          cloneEnv' xs envRef

getEnv :: Env -> StashEnvs -> String -> SourcePos -> IOThrowsError Env
getEnv env stashEnvs name sp = do
  envs <- liftIO $ readIORef stashEnvs
  case lookup name envs of
    Nothing -> do
      ei <-  errorInfo env sp
      throwError $ BitesTheNothing name ei
    Just env -> return env

evalBitesTheDust :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalBitesTheDust envRef stashEnvs (BitesTheDust name sp) = do
  stashedEnv <- getEnv envRef stashEnvs name sp
  liftIO $ do
    env <- readIORef stashedEnv
    writeIORef envRef env
    return None

evalTheHand :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalTheHand envRef stashEnvs (TheHand varName _) =
  liftIO $ do env <- readIORef envRef
              writeIORef envRef (removeVar varName env)
              return None
  where removeVar :: String -> [Mut] -> [Mut]
        removeVar _ [] = []
        removeVar varName (x:xs) | varName == mName x = removeVar varName xs
                                 | otherwise = x : removeVar varName xs

evalPart :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalPart env stashEnvs (Part partName sp) = do
  LangString filename <- getFilename env sp
  if filename == partName
    then return None
    else throw env (FilePartDontMatch filename partName) sp

getPart :: Env -> SourcePos -> IOThrowsError BizVal
getPart env sp = getVar env "__part__" sp

getFilename :: Env -> SourcePos -> IOThrowsError BizVal
getFilename env sp = getVar env "__filename__" sp

evalSkip :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalSkip env stashEnvs (SkipPart part maybeAs sp) = do
  LangString currentPart <- getPart env sp
  setVar env "__filename__" (LangString part) []
  res <- load env part sp
  case head res of
    Part _ _ -> do
      case maybeAs of
        Nothing -> setVar env "__part__" (LangString part) []
        Just as -> setVar env "__part__" (LangString as) []
      mapM_ (eval env stashEnvs) res
      setVar env "__part__" (LangString currentPart) []
      return None
    _ -> throw env MissingPartDeclaration sp

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "biz" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

throw env bizError sp = do
  ei <- errorInfo env sp
  throwError $ bizError ei

errorInfo :: Env -> SourcePos -> IOThrowsError ErrorInfo
errorInfo env sp = do
  (LangString path) <- getVar env "__fullname__" sp
  return $ ErrorInfo path sp

-- Env

nullEnv :: IO Env
nullEnv = newIORef []

nullStashEnvs :: IO (IORef [(String, Env)])
nullStashEnvs = newIORef []

isBound :: Env -> String -> SourcePos -> IOThrowsError Bool
isBound envRef var sp = do
  (String part _) <- getPart envRef sp
  env <- liftIO $ readIORef envRef
  case luPart var part env of
    Just _ -> return True
    _ -> return False

lu :: String -> [Mut] -> [Mut]
lu _ [] = []
lu varName (x:xs) =
  if mName x == varName
  then x : lu varName xs
  else lu varName xs

luPart :: String -> String -> [Mut] -> Maybe Mut
luPart _ _ [] = Nothing
luPart name part (x:xs) =
  if mName x == name && mPart x == part
  then Just x
  else luPart name part xs

getVar :: Env -> String -> SourcePos -> IOThrowsError BizVal
getVar envRef var sp =
  do env <- liftIO $ readIORef envRef
     let list = wordsWhen (== '.') var
     if length list == 1 -- There isn't a '.' in the atom
       then case lu (head list) env of
              [] -> throw envRef (UnboundName var) sp
              [mut] -> liftIO $ readIORef $ mValue mut
              manyVars -> do maybeVar <- getVar' envRef manyVars sp
                             case maybeVar of
                               Nothing -> throw envRef (SameNameDifferentParts var) sp
                               Just mut -> liftIO $ readIORef $ mValue mut
       else let part = intercalate "." . take (length list - 1) $ list
            in do String filename _ <- getFilename envRef sp
                  if part == filename
                    then getVar envRef (last list) sp
                    else case luPart (last list) part env of
                           Nothing -> throw envRef (UnboundName var) sp
                           Just thing -> liftIO $ readIORef $ mValue thing

getVar' :: Env -> [Mut] -> SourcePos -> IOThrowsError (Maybe Mut)
getVar' _ [] sp = return Nothing
getVar' env (x:xs) sp = do (String part _) <- getPart env sp
                           if part == mPart x
                            then return (Just x)
                            else getVar' env xs sp

getVarAttributes :: Env -> String -> SourcePos -> IOThrowsError [VarAttribute]
getVarAttributes envRef var sp = do env <- liftIO $ readIORef envRef
                                    case lu var env of
                                      [] -> throw envRef (UnboundName var) sp
                                      [thing] -> liftIO $ readIORef $ mAttributes thing
                                      _ -> throw envRef (SameNameDifferentParts var) sp

setVar :: Env -> String -> BizVal -> [VarAttribute] -> IOThrowsError BizVal
setVar envRef var value attrs = do env <- liftIO $ readIORef envRef
                                   case lu var env of
                                     [] -> throw envRef (UnboundName var) (sourcePos value)
                                     [someVar] -> liftIO $ do
                                       writeIORef (mValue someVar) value
                                       writeIORef (mAttributes someVar) attrs
                                       return value
                                     _ -> throw envRef (SameNameDifferentParts var) (sourcePos value)

setVarPart :: Env -> String -> BizVal -> [VarAttribute] -> String -> IOThrowsError BizVal
setVarPart envRef var value attrs part = do env <- liftIO $ readIORef envRef
                                            case luPart var part env of
                                              Nothing -> throw envRef (UnboundName var) (sourcePos value)
                                              Just someVar -> liftIO $ do
                                                writeIORef (mValue someVar) value
                                                writeIORef (mAttributes someVar) attrs
                                                return value

defineVar :: Env -> String -> BizVal -> [VarAttribute] -> IOThrowsError BizVal
defineVar envRef var value attributes = do
  (String part _) <- getPart envRef (sourcePos value)
  defineVar' envRef var value attributes part

defineVar' :: Env -> String -> BizVal -> [VarAttribute] -> String -> IOThrowsError BizVal
defineVar' envRef var value attributes part = do
  alreadyDefined <- isBound envRef var (sourcePos value)
  if alreadyDefined
    then do
    attrs <- getVarAttributes envRef var (sourcePos value)
    (if Reliable `elem` attrs
     then throw envRef (ReliableName var) (sourcePos value)
     else do
        setVar envRef var value attributes
        return value
      )
    else liftIO $ do valueRef <- newIORef value
                     attrsRef <- newIORef attributes
                     env <- readIORef envRef
                     writeIORef envRef (Mut var valueRef attrsRef part : env)
                     return value

bindVars :: Env -> [(String, BizVal)] -> String -> IO Env
bindVars envRef bindings part = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do valueRef <- newIORef value
                                     attrsRef <- newIORef []
                                     return $ Mut var valueRef attrsRef part