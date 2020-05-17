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
import Data.IORef
import Data.Either (fromRight)
import Data.List (isPrefixOf, findIndex, tails, intersperse, intercalate)
import qualified Data.Map as M
import Data.Maybe (isJust)
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.FilePath as FP

import Biz.Parser (expr, spaces1, topLevel, eeol)
import Biz.Data
import Biz.Helper

readExprList = readOrThrow $ (sepEndBy (spaces >> topLevel) (try $ many (char ' ' <|> char '\t') >> eeol))

eval :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
eval _ _ val@(String _) = return val
eval _ _ val@(Number _) = return val
eval _ _ val@(Bool _) = return val
eval env _ (Atom id) = getVar env id
eval env stashEnvs val@(Var (Atom name) value attributes) = evalVar env stashEnvs val
eval env stashEnvs val@If {} = evalIf env stashEnvs val
eval env stashEnvs val@(Seq _) = evalSeq env stashEnvs val 0
eval env stashEnvs val@(FuncCall _ _) = evalFuncCall env stashEnvs val
eval env stashEnvs val@(Funcf (Atom name) params body) = evalFuncDef env stashEnvs val
eval env stashEnvs val@(HolHorse _ _) = evalHolHorse env stashEnvs val
eval env stashEnvs val@ForLoop {} = evalForLoop env stashEnvs val
eval env stashEnvs val@DoppioLoop {} = evalDoppioLoop env stashEnvs val
eval env stashEnvs val@(InfiniteLoop _) = evalInfiniteLoop env stashEnvs val
eval env stashEnvs val@(Arrivederci _) = return val

eval env stashEnvs KingCrimson = return KingCrimson
eval env stashEnvs val@(MoodyBlues _) = return val
eval env stashEnvs val@(Mista _) = undefined
eval env stashEnvs val@(Doppio _) = return val
eval env stashEnvs val@Dururu {} = evalDururu env stashEnvs val
eval env stashEnvs val@Rurudu {} = evalRurudu env stashEnvs val
eval _ _ val@(User _ _) = return val
eval _ _ val@(Stand _ _) = return val
eval env stashEnvs val@(Cry _) = evalCry env stashEnvs val
eval env stashEnvs val@(Arrow _ _) = evalArrow env stashEnvs val
eval env stashEnvs val@(Ability _ _) = evalAbility env stashEnvs val
eval env stashEnvs val@(SingleArrow _) = undefined
eval env stashEnvs val@(KillerQueen name) = evalKillerQueen env stashEnvs val
eval env stashEnvs val@(BitesTheDust name) = evalBitesTheDust env stashEnvs val
eval env stashEnvs val@(TheHand name) = evalTheHand env stashEnvs val
eval env stashEnvs val@(Part _) = evalPart env stashEnvs val
eval env stashEnvs val@(SkipPart _ _) = evalSkip env stashEnvs val
eval env stashEnvs val@GirlsDontLikeGuys {} = evalGirlsDontLikeGuys env stashEnvs val
eval env stashEnvs val@(PorcaMiseria _ _) = evalPorcaMiseria env stashEnvs val
eval env stashEnvs val@(Cazzo _) = evalCazzo env stashEnvs val
eval env stashEnvs val@(Crepa _) = return val

evalVar :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalVar env stashEnvs (Var (Atom name) value attributes) = do
  bv <- eval env stashEnvs value
  case bv of
    None -> throwError NoneIsUnbindable
    _ -> defineVar env name bv attributes

evalIf :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalIf env stashEnvs (If pred maybeLeft maybeRight maybeBoth) =
  do result <- eval env stashEnvs pred
     case result of
       Bool False -> case (maybeLeft, maybeBoth) of
                      (Nothing, Nothing) -> return None
                      (Just left, Nothing) -> eval env stashEnvs left
                      (Just left, Just both) -> do eval env stashEnvs left
                                                   eval env stashEnvs both
                      (Nothing, Just both) -> eval env stashEnvs both
       Bool True -> case (maybeRight, maybeBoth) of
                       (Nothing, Nothing) -> return None
                       (Just right, Nothing) -> eval env stashEnvs right
                       (Just right, Just both) -> do eval env stashEnvs right
                                                     eval env stashEnvs both
                       (Nothing, Just both) -> eval env stashEnvs both
       notBool -> throwError $ TypeMismatch "boolean" notBool

evalSeq :: Env -> StashEnvs -> BizVal -> Integer -> IOThrowsError BizVal
evalSeq env stashEnvs (Seq []) _ = return None
evalSeq env stashEnvs (Seq [x]) _ = do
  result <- eval env stashEnvs x
  case result of
    Arrivederci expr -> eval env stashEnvs expr
    KingCrimson -> eval env stashEnvs KingCrimson
    MoodyBlues _ -> return None
    _ -> return result
evalSeq env stashEnvs (Seq exps) n = do
  result <- eval env stashEnvs (exps !! fromInteger n)
  case result of
    Arrivederci expr -> eval env stashEnvs expr
    KingCrimson -> eval env stashEnvs KingCrimson
    MoodyBlues number -> moodyHelper env stashEnvs number n exps
    _ -> if fromInteger (n + 1) < length exps
         then evalSeq env stashEnvs (Seq exps) (n + 1)
         else return result
  where moodyHelper env stashEnvs number n exps
          | number == 0 = return None
          | number <= n = evalSeq env stashEnvs (Seq $ delete (fromInteger n) exps) (n - number)
          | otherwise = let newList = delete (fromInteger n) exps
                            newLength = (toInteger $ length newList)
                        in evalSeq env stashEnvs (Seq newList) $ newLength - 1 - (number `mod` newLength)

evalFuncDef :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalFuncDef env stashEnvs (Funcf (Atom name) params body) = do
  let fun = Func (map (\(Atom n) -> n) params) body env stashEnvs
  defineVar env name fun []

evalAbility :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalAbility env stashEnvs (Ability name value) = do
  evalVal <- eval env stashEnvs value
  return (Ability name evalVal)

evalCry :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalCry env stashEnvs (Cry [a@(Atom name)]) = eval env stashEnvs a
evalCry env stashEnvs (Cry [value]) = return value
evalCry env stashEnvs (Cry (bizVal:(Atom a2):xs)) = do
  v <- eval env stashEnvs bizVal
  case v of
    User uName stands -> do
      case M.lookup a2 stands of
        Nothing -> throwError $ FieldDoesntExist uName a2
        Just stand -> evalCry env stashEnvs (Cry (stand:xs))        
    Stand sName abilities -> do
      case M.lookup a2 abilities of
        Nothing -> throwError $ FieldDoesntExist sName a2
        Just ability -> evalCry env stashEnvs (Cry (ability:xs))  

evalArrow :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalArrow env stashEnvs (Arrow atom1 atom2) = do
  value1 <- eval env stashEnvs atom1 
  value2 <- eval env stashEnvs atom2
  case (value1, value2) of
    (s@(Stand sName sAbilities) , User userName stands) ->
      defineVar env userName (User userName $ M.insertWith mergeStands sName s stands) []
    (Ability aName aValue, Stand standName abilities) -> return (Stand standName $ M.insert aName aValue abilities)
    _ -> throwError $ WrongArrow value1 value2

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

load :: Env -> String -> IOThrowsError [BizVal]
load env part = do
  (String curDir) <- getVar env "__mainpath__"
  let filename = curDir </> replace '.' '/' part <.> ".bz"
  content <- liftIO (CE.try $ readFile filename :: IO (Either CE.SomeException String)) 
  case content of
    Left _ -> do let fn = curDir ++ replace '.' '/' part ++ ".biz"
                 content <- liftIO (CE.try $ readFile filename :: IO (Either CE.SomeException String))
                 case content of
                   Left _ -> throwError $ PartDoesntExist part
                   Right r -> readContent r
    Right r -> readContent r
  where readContent r = do let clear = unlines . clearComments . lines $ r
                           liftThrows . readExprList $ clear

evalLoad :: Env -> StashEnvs -> String -> IOThrowsError BizVal
evalLoad env stashEnvs filename = do
  setVar env "__mainpath__" (String $ FP.takeDirectory filename) []
  let ext = takeExtension filename
    in
    if ext == ".bz" || ext == ".biz"
    then evalSkip env stashEnvs (SkipPart (FP.takeBaseName filename) Nothing)
    else throwError $ WrongBizFile filename

evalDururu :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalDururu env stashEnvs (Dururu number moshi bv) = do
  result <- eval env stashEnvs bv
  case result of
    Doppio list -> case moshi of
                     Nothing -> return $ list !! fromInteger number
                     Just val -> return $ Doppio $ replaceNth (fromInteger number) val list
    notDoppio -> throwError $ TypeMismatch "doppio" notDoppio

evalRurudu :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalRurudu env stashEnvs (Rurudu number moshi bv) = do
  result <- eval env stashEnvs bv
  case result of
    Doppio list -> case moshi of
                     Nothing -> return $ reverse list !! fromInteger number
                     Just val -> return $ Doppio $ reverse . replaceNth (fromInteger number) val $ reverse list
    notDoppio -> throwError $ TypeMismatch "doppio" notDoppio

evalHolHorse :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalHolHorse env stashEnvs val@(HolHorse params body) =
  return $ Func (map (\(Atom n) -> n) params) body env stashEnvs

evalFuncCall :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalFuncCall env stashEnvs (FuncCall name args) = do
  func <- eval env stashEnvs name
  argVals <- mapM (eval env stashEnvs) args
  apply func argVals

apply :: BizVal -> [BizVal] -> IOThrowsError BizVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params body closure stashEnvs) args =
  if num params /= num args
  then throwError $ NumArgs (num params) 456
  else do
    (String part) <- getPart closure
    prepareEnv closure
    env <- liftIO $ bindVars closure (zip params args) part
    result <- eval env stashEnvs body
    case result of
      Arrivederci expr -> eval env stashEnvs expr
      _ -> return result
  where num = toInteger . length

evalInfiniteLoop :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalInfiniteLoop env stashEnvs val@(InfiniteLoop body) = do
  result <- eval env stashEnvs body
  case result of
    Arrivederci expr -> eval env stashEnvs expr
    _ -> evalInfiniteLoop env stashEnvs val

evalForLoop :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalForLoop env stashEnvs (ForLoop (Atom localVar) start inc predicate body) = do
  evalStart <- eval env stashEnvs start  
  localEnv <- do
    (String part) <- getPart env
    liftIO $ bindVars env [(localVar, evalStart)] part
  evalInc <- eval localEnv stashEnvs inc
  evalPred <- eval localEnv stashEnvs predicate
  case (evalStart, evalInc, evalPred) of
    (Number s, Number i, Bool b) ->
      case evalPred of
        Bool True -> do
          result <- eval localEnv stashEnvs body
          case result of
            Arrivederci expr -> eval localEnv stashEnvs expr
            _ -> evalForLoop localEnv stashEnvs (ForLoop (Atom localVar) (Number $ s + i) inc predicate body)
        _ -> eval env stashEnvs (Bool True)
        
    (Number _, Number _, _) -> throwError $ TypeMismatch "bool" evalPred
    (Number _, _, _) -> throwError $ TypeMismatch "number" evalInc
    (_, _, _) -> throwError $ TypeMismatch "number" evalStart
    
evalDoppioLoop :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalDoppioLoop env stashEnvs loop@(DoppioLoop (Atom localVar) doppio body) = do
  bv <- eval env stashEnvs doppio
  case bv of
    Doppio [] -> return None
    Doppio (x:xs) -> do (String part) <- getPart env
                        localEnv <- liftIO $ bindVars env [(localVar, x)] part
                        result <- eval localEnv stashEnvs body
                        case result of
                          (Arrivederci expr) -> eval localEnv stashEnvs expr
                          notArr -> if length xs == 0
                            then return notArr
                            else evalDoppioLoop env stashEnvs (DoppioLoop (Atom localVar) (Doppio xs) body)
    notDoppio -> throwError $ TypeMismatch "doppio" bv

evalGirlsDontLikeGuys :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalGirlsDontLikeGuys env stashEnvs (GirlsDontLikeGuys expVal predicate items) = do
  evalPred <- eval env stashEnvs predicate
  case evalPred of
    PrimitiveFunc _ -> do
      val <- eval env stashEnvs expVal
      evalGirl' env stashEnvs evalPred val items
    Func _ _ _ _ -> do
      val <- eval env stashEnvs expVal
      evalGirl' env stashEnvs evalPred val items
    notPred -> throwError $ GirlsLikePredicates notPred
      
  where evalGirl' :: Env -> StashEnvs -> BizVal -> BizVal -> [BizVal] -> IOThrowsError BizVal
        evalGirl' _ _ _ _ [] = return None
        evalGirl' env stashEnvs pred val ((GirlItem value effect):xs) =
          case value of
            Atom "steal" -> eval env stashEnvs effect
            bizVal -> do bv <- eval env stashEnvs bizVal
                         res <- apply pred [val, bv]
                         case res of
                           Bool True -> eval env stashEnvs effect
                           Bool False -> evalGirl' env stashEnvs pred val xs
                           notBool -> throwError $ GirlsLikePredicates notBool


evalPorcaMiseria :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalPorcaMiseria env stashEnvs (PorcaMiseria origin bizVal) = do
  catchError
    (eval env stashEnvs bizVal)
    (\bizError -> evalFuncCall env stashEnvs (FuncCall (Atom "whoWroteAllThis") [String origin, Crepa bizError]))

evalCazzo :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalCazzo env stashEnvs (Cazzo error) = do
  e <- eval env stashEnvs error
  case e of
    Crepa bizError -> throwError bizError
    notCrepa -> throwError $ CazzoWithoutCrepa notCrepa

prepareEnv :: Env -> IOThrowsError Env
prepareEnv envRef = do
  env <- liftIO $ readIORef envRef
  constEnv envRef env

constEnv :: Env -> [Mut] -> IOThrowsError Env
constEnv env [] = return env
constEnv env (x:xs) = do
  va <- liftIO $ readIORef $ attributes x
  bizVal <- liftIO $ readIORef $ mValue x
  if Reliable `elem` va || NoDignity `elem` va
    then constEnv env xs
    else do setVarPart env (mName x) bizVal (Reliable : va) (mPart x)
            constEnv env xs

isStashEnvBound :: StashEnvs -> String -> IO Bool
isStashEnvBound envRef var = isJust . lookup var <$> readIORef envRef

evalKillerQueen :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalKillerQueen env stashEnvs (KillerQueen (Atom name)) = do
  bound <- liftIO $ isStashEnvBound stashEnvs name
  if bound
    then do boundEnv <- getEnv stashEnvs name
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
          attrs <- readIORef $ attributes x
          attrsRef <- newIORef attrs
          env <- readIORef envRef
          writeIORef envRef (Mut (mName x) valueRef attrsRef (mPart x) : env)
          cloneEnv' xs envRef

getEnv :: StashEnvs -> String -> IOThrowsError Env
getEnv stashEnvs name = do
  env <- liftIO $ readIORef stashEnvs
  case lookup name env of
    Nothing -> throwError $ BitesTheNothing name
    Just env -> return env

evalBitesTheDust :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalBitesTheDust envRef stashEnvs (BitesTheDust (Atom name)) = do
  stashedEnv <- getEnv stashEnvs name
  liftIO $ do
    env <- readIORef stashedEnv
    writeIORef envRef env
    return None

evalTheHand :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalTheHand envRef stashEnvs (TheHand (Atom var)) =
  liftIO $ do env <- readIORef envRef
              writeIORef envRef (removeVar var env)
              return None
  where removeVar :: String -> [Mut] -> [Mut]
        removeVar _ [] = []
        removeVar varName (x:xs) | varName == mName x = removeVar var xs
                                 | otherwise = x : removeVar var xs

evalPart :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalPart env stashEnvs (Part partName) = do
  String filename <- getFilename env
  if filename == partName
    then return None
    else throwError $ FilePartDontMatch filename partName
    
getPart :: Env -> IOThrowsError BizVal
getPart env = getVar env "__part__"

getFilename :: Env -> IOThrowsError BizVal
getFilename env = getVar env "__filename__"

evalSkip :: Env -> StashEnvs -> BizVal -> IOThrowsError BizVal
evalSkip env stashEnvs (SkipPart part maybeAs) = do
  String currentPart <- getPart env
  setVar env "__filename__" (String part) []
  res <- load env part
  case head res of
    Part _ -> do case maybeAs of
                   Nothing -> setVar env "__part__" (String part) []
                   Just as -> setVar env "__part__" (String as) []
                 mapM_ (eval env stashEnvs) res
                 setVar env "__part__" (String currentPart) []
                 return None
    _ -> throwError MissingPartDeclaration

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "biz" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

-- Env

nullEnv :: IO Env
nullEnv = newIORef []

nullStashEnvs :: IO (IORef [(String, Env)])
nullStashEnvs = newIORef []

isBound :: Env -> String -> IOThrowsError Bool
isBound envRef var = do
  (String part) <- getPart envRef
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

getVar :: Env -> String -> IOThrowsError BizVal
getVar envRef var =
  do env <- liftIO $ readIORef envRef
     let list = wordsWhen (== '.') var
     if length list == 1 -- There isn't a '.' in the atom
       then case lu (head list) env of
              [] -> throwError $ UnboundName var
              [mut] -> liftIO $ readIORef $ mValue mut
              manyVars -> do maybeVar <- getVar' envRef manyVars
                             case maybeVar of
                               Nothing -> throwError $ SameNameDifferentParts var
                               Just mut -> liftIO $ readIORef $ mValue mut
       else let part = intercalate "." . take (length list - 1) $ list
            in do String filename <- getFilename envRef
                  if part == filename
                    then getVar envRef (last list)
                    else case luPart (last list) part env of
                           Nothing -> throwError $ UnboundName var
                           Just thing -> liftIO $ readIORef $ mValue thing

getVar' :: Env -> [Mut] -> IOThrowsError (Maybe Mut)
getVar' _ [] = return Nothing
getVar' env (x:xs) = do (String part) <- getPart env
                        if part == mPart x
                          then return (Just x)
                          else getVar' env xs
                                       
getVarAttributes :: Env -> String -> IOThrowsError [VarAttribute]
getVarAttributes envRef var = do env <- liftIO $ readIORef envRef
                                 case lu var env of
                                   [] -> throwError $ UnboundName var
                                   [thing] -> liftIO $ readIORef $ attributes thing
                                   _ -> throwError $ SameNameDifferentParts var
                                   
setVar :: Env -> String -> BizVal -> [VarAttribute] -> IOThrowsError BizVal
setVar envRef var value attrs = do env <- liftIO $ readIORef envRef
                                   case lu var env of
                                     [] -> throwError $ UnboundName var
                                     [someVar] -> liftIO $ do
                                       writeIORef (mValue someVar) value
                                       writeIORef (attributes someVar) attrs
                                       return value
                                     _ -> throwError $ SameNameDifferentParts var

setVarPart :: Env -> String -> BizVal -> [VarAttribute] -> String -> IOThrowsError BizVal
setVarPart envRef var value attrs part = do env <- liftIO $ readIORef envRef
                                            case luPart var part env of
                                              Nothing -> throwError $ UnboundName var
                                              Just someVar -> liftIO $ do
                                                writeIORef (mValue someVar) value
                                                writeIORef (attributes someVar) attrs
                                                return value

defineVar :: Env -> String -> BizVal -> [VarAttribute] -> IOThrowsError BizVal
defineVar envRef var value attributes = do
  (String part) <- getPart envRef
  defineVar' envRef var value attributes part

defineVar' :: Env -> String -> BizVal -> [VarAttribute] -> String -> IOThrowsError BizVal
defineVar' envRef var value attributes part = do
  alreadyDefined <- isBound envRef var
  if alreadyDefined
    then do
    attrs <- getVarAttributes envRef var
    (if Reliable `elem` attrs
     then throwError $ ReliableName var
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
