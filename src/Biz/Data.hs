module Biz.Data
  ( BizVal(..)
  , BizError(..)
  , Env
  , StashEnvs
  , ThrowsError
  , IOThrowsError
  , extractValue
  , liftThrows
  , VarAttribute(..)
  , Mut(..)
  , ErrorInfo(..)
  )
where

import           Text.ParserCombinators.Parsec  ( ParseError )
import           Text.ParserCombinators.Parsec.Pos
import           Control.Monad.Except           ( ExceptT
                                                , throwError
                                                )
import           System.IO                      ( Handle )
import           Data.IORef                     ( IORef )

import qualified Data.Map                      as M
import qualified Data.List                     as L

data Mut = Mut { mName :: String
               , mValue :: IORef BizVal
               , mAttributes :: IORef [VarAttribute]
               , mPart :: String
               }

type Env = IORef [Mut]
type StashEnvs = IORef [(String, Env)]

data BizVal = Atom { name :: String, sourcePos :: SourcePos}
            | Number { number :: Double, sourcePos :: SourcePos}
            | String { text :: String, sourcePos :: SourcePos}
            | Bool { bool :: Bool, sourcePos :: SourcePos}
            | FuncCall { atomName :: BizVal, args :: [BizVal], sourcePos :: SourcePos}
            | If { predicate :: BizVal, right :: (Maybe BizVal), left :: (Maybe BizVal), both :: (Maybe BizVal), sourcePos :: SourcePos}
            | Var {atomName :: BizVal, value :: BizVal, attributes :: [VarAttribute], sourcePos :: SourcePos }
            | Seq {exprs :: [BizVal], sourcePos :: (SourcePos) }
            | PrimitiveFunc ([BizVal] -> ErrorInfo -> ThrowsError BizVal)
            | Funcf { func :: BizVal, paramss :: [BizVal], body :: BizVal, sourcePos :: SourcePos }
            | Func {params :: [String], body :: BizVal, closure :: Env , stashEnvs :: StashEnvs, sourcePos :: SourcePos}
            | IOFunc ([BizVal] -> ErrorInfo -> IOThrowsError BizVal)
            | Port Handle (SourcePos)
            | Arrivederci { value :: BizVal, sourcePos :: SourcePos }
            | DoppioLoop {localVar :: BizVal, doppio :: BizVal, body :: BizVal, sourcePos :: SourcePos }
            | InfiniteLoop {body :: BizVal, sourcePos :: SourcePos }
            | ForLoop {localVar :: BizVal, start :: BizVal, increment :: BizVal, predicate :: BizVal, body :: BizVal, sourcePos :: SourcePos }
            | Mista BizVal SourcePos
            | KingCrimson { sourcePos :: SourcePos }
            | Doppio { list :: [BizVal], sourcePos :: SourcePos }
            | Dururu { count :: Integer, moshi :: (Maybe BizVal), doppio :: BizVal, sourcePos :: SourcePos }
            | Rurudu { count :: Integer, moshi :: (Maybe BizVal), doppio :: BizVal, sourcePos :: SourcePos }
            | Part { name :: String, sourcePos :: SourcePos }
            | SkipPart { name :: String, newName :: (Maybe String), sourcePos :: SourcePos }
            | User { name :: String, stands :: (M.Map String BizVal), sourcePos :: SourcePos }
            | Stand { name :: String, abilities :: (M.Map String BizVal), sourcePos :: SourcePos }
            | Ability { name :: String, value :: BizVal, sourcePos :: SourcePos }
            | SingleArrow BizVal (SourcePos)
            | Arrow { sourcePos :: SourcePos, leftOperand :: BizVal, rightOperand :: BizVal }
            | Cry { list :: [BizVal], sourcePos :: SourcePos }
            | HolHorse { params :: [String], body :: BizVal, sourcePos :: SourcePos }
            | KillerQueen { name :: String, sourcePos :: SourcePos }
            | BitesTheDust { name :: String, sourcePos :: SourcePos }
            | TheHand { name :: String, sourcePos :: SourcePos }
            | None
            | MoodyBlues {count :: Integer, sourcePos :: SourcePos }
            | GirlsDontLikeGuys { value :: BizVal, predicate :: BizVal, items :: [BizVal], sourcePos :: SourcePos }
            | GirlItem { value :: BizVal, effect :: BizVal, sourcePos :: SourcePos }
            | PorcaMiseria { name :: String, value :: BizVal, sourcePos :: SourcePos }
            | Cazzo { crepa :: BizVal, sourcePos :: SourcePos }
            | Crepa { errorName :: String, maybeVal :: Maybe BizVal, sourcePos :: SourcePos }
            | CrepaBizError BizError SourcePos
            | LangList [BizVal]
            | LangString String

showVal :: BizVal -> String
showVal (Atom name _) = name
showVal (Number n _) =
  if n == fromInteger (round n) then (show . round $ n) else show n
showVal (String        s     _) = "\"" ++ s ++ "\""
showVal (Bool          True  _) = "yes"
showVal (Bool          False _) = "no"
showVal (Var name _ _ _) = "kono " ++ show name
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func { params = args, body = body, closure = env, stashEnvs = stashEnvs }
  = "func "
showVal (IOFunc      _) = "<IO primitive>"
showVal (Arrivederci val _  ) = "arrivederci " ++ show val
showVal (DoppioLoop _ _ _ _ ) = "<list loop>"
showVal (InfiniteLoop _ _   ) = "<infinite loop"
showVal (ForLoop _ _ _ _ _ _) = "<for loop>"
showVal (Mista bv _         ) = "<mista " ++ show bv ++ ">"
showVal (KingCrimson _      ) = "King Crimson"
showVal (Doppio list _) = "dop " ++ (unwords . map showVal $ list) ++ " pio"
showVal (Seq    _    _      ) = "<seq>"
showVal (Stand name abilities _) =
  "Stand "
    ++ name
    ++ " "
    ++ (L.intercalate ", " $ Prelude.map printPair $ M.toList abilities)
showVal (User user stands _) =
  "User "
    ++ user
    ++ " "
    ++ (L.intercalate "; " $ Prelude.map (show . snd) $ M.toList stands)
showVal (Cry list _           ) = concatMap ((++) " !" . show) list
showVal (Arrow first second _ ) = show first ++ " -> " ++ show second
showVal (SingleArrow target _ ) = "-> " ++ show target
showVal (Ability  name value _) = "Ability " ++ name ++ " " ++ show value
showVal (HolHorse _    _     _) = "holhorse"
showVal (KillerQueen name _   ) = "Killer Queen: " ++ name
showVal (Funcf _ _ _ _        ) = "func def"
showVal (MoodyBlues n    _    ) = "Moody Blues " ++ show n
showVal (Part       name _    ) = "Part " ++ name
showVal _                       = "none"

printPair p = fst p ++ " " ++ (show $ snd p)

eq :: BizVal -> BizVal -> Bool
eq (Number n1 _     ) (Number n2 _     ) = n1 == n2
eq (String s1 _     ) (String s2 _     ) = s1 == s2
eq (Bool   b1 _     ) (Bool   b2 _     ) = b1 == b2
eq (Doppio d1 _     ) (Doppio d2 _     ) = d1 == d2
eq (User    n1 s1  _) (User    n2 s2  _) = n1 == n2 && s1 == s2
eq (Stand   n1 a1  _) (Stand   n2 a2  _) = n1 == n2 && a1 == a2
eq (Ability n1 bv1 _) (Ability n2 bv2 _) = n1 == n2 && bv1 == bv2
--eq (Crepa be1 _ _  _  ) (Crepa be2 _  _   ) = be1 == be2
eq _                  _                  = False


traceValue :: BizVal -> String
traceValue (Atom a _) = a
traceValue (Number n _) = show n
traceValue (String s _) = show s
traceValue (Bool b _) = show b
traceValue (FuncCall func args _) = "oingo " ++ traceValue func
traceValue (Var name value attrs _) = "kono " ++ traceValue name
traceValue (Arrivederci expr _) = "arrivederci " ++ traceValue expr
traceValue (DoppioLoop _ _ _ _) = "ger"
traceValue (InfiniteLoop _ _) = "ger"
traceValue (ForLoop _ _ _ _ _ _) = "ger"
traceValue (User name _ _) = "user " ++ name
traceValue (Stand name _ _) = "stand " ++ name
traceValue a@(Ability _ _ _) = show a
traceValue (KillerQueen name _) = "killer queen " ++ name
traceValue (BitesTheDust name _) = "bites the dust " ++ name
traceValue _                  = ""

instance Show BizVal where
  show = showVal
instance Eq BizVal where
  (==) = eq

unwordsList :: [BizVal] -> String
unwordsList = unwords . map showVal

data BizError = Parser ParseError
              | NumArgs Integer Integer ErrorInfo
              | TypeMismatch String BizVal ErrorInfo
              | UnboundName String ErrorInfo
              | WrongArrow BizVal BizVal ErrorInfo
              | FieldDoesntExist String String ErrorInfo
              | ErrorCry BizVal BizVal ErrorInfo
              | ReliableName String ErrorInfo
              | NoneIsUnbindable ErrorInfo
              | BitesTheNothing String ErrorInfo
              | PartDoesntExist String ErrorInfo
              | MissingPartDeclaration ErrorInfo
              | FilePartDontMatch String String ErrorInfo
              | SameNameDifferentParts String ErrorInfo
              | WrongBizFile String ErrorInfo
              | CazzoWithoutCrepa BizVal ErrorInfo
              | CrepaWithoutPeli BizVal ErrorInfo
              | GirlsLikePredicates BizVal ErrorInfo
              | CustomError String (Maybe BizVal) ErrorInfo

showError :: BizError -> String
showError (Parser err) = show err 
showError (NumArgs expected found sp) = "Expected " ++ show expected ++ " arguments. Found " ++ show found ++ showSourcePos sp
showError (TypeMismatch expected found sp) = "Invalid type: expected " ++ expected ++ ". Found " ++ show found ++ showSourcePos sp
showError (UnboundName name sp) = "The name " ++ show name ++ " is unbound" ++ showSourcePos sp
showError (WrongArrow bv1 bv2 sp) = "Wrong arrow. You can't use it with " ++ show bv1 ++ " and " ++ show bv2  ++ showSourcePos sp
showError (FieldDoesntExist value field sp) = show value ++ "\" doesn't have a field with the name \"" ++ field ++ "\"" ++ showSourcePos sp
showError (ErrorCry bv1 bv2 sp) = "Wrong cry. You can't use it with" ++ show bv1 ++ " and " ++ show bv2 ++ showSourcePos sp
showError (ReliableName var sp) = "The name \"" ++ var ++ "\" is reliable. You can't update it" ++ showSourcePos sp
showError (NoneIsUnbindable sp) = "none is unbindable" ++ showSourcePos sp
showError (BitesTheNothing name sp) = "Bites the dust can't be activated because \"" ++ name ++ "\" doesn't exist" ++ showSourcePos sp
showError (PartDoesntExist part sp) = "Part \"" ++ part ++ "\" doesn't exist" ++ showSourcePos sp
showError (FilePartDontMatch fileName part sp) ="Filename and part name don't match. Filename: " ++ fileName ++ ". Part: " ++ part ++ showSourcePos sp
showError (SameNameDifferentParts name sp) = "The name \"" ++ name ++ "\" is bond in different parts" ++ showSourcePos sp
showError (WrongBizFile filename sp) = "File \"" ++ filename ++ "\" isn't a Biz file" ++ showSourcePos sp
showError (CazzoWithoutCrepa notCrepa sp) = "Every cazzo needs a crepa. " ++ show notCrepa ++ " was provided" ++ showSourcePos sp
showError (CrepaWithoutPeli crepa sp) = "Crepa " ++ show crepa ++ "without peli" ++ showSourcePos sp
showError (GirlsLikePredicates notPred sp) = "Girls like guys who use predicates. \"" ++ show notPred ++ "\" is provided" ++ showSourcePos sp
showError (CustomError name maybeData sp) = name ++ maybePrint maybeData ++ showSourcePos sp
 where
  maybePrint a = case a of
    Nothing -> ""
    Just v  -> ": " ++ show v

errorSourcePos :: BizError -> ErrorInfo
errorSourcePos (NumArgs _ _ sp) = sp
errorSourcePos (TypeMismatch _ _ sp) = sp
errorSourcePos (WrongArrow _ _ sp) = sp
errorSourcePos (FieldDoesntExist _ _ sp) = sp
errorSourcePos (ErrorCry _ _ sp) = sp
errorSourcePos (ReliableName _ sp) = sp
errorSourcePos (NoneIsUnbindable sp) = sp
errorSourcePos (BitesTheNothing _ sp) = sp
errorSourcePos (PartDoesntExist _ sp) = sp
errorSourcePos (FilePartDontMatch _ _ sp) = sp
errorSourcePos (SameNameDifferentParts _ sp) = sp
errorSourcePos (WrongBizFile _ sp) = sp
errorSourcePos (CazzoWithoutCrepa _ sp) = sp
errorSourcePos (CrepaWithoutPeli _ sp) = sp
errorSourcePos (GirlsLikePredicates _ sp) = sp
errorSourcePos (CustomError _ _ sp) = sp

showSourcePos :: ErrorInfo -> String
showSourcePos (ErrorInfo path sp) = ". " ++ path ++ " at line " ++ show (sourceLine sp)

eqError :: BizError -> BizError -> Bool
eqError (CustomError name1 _ _) (CustomError name2 _ _) = name1 == name2
eqError _                     _                     = True

instance Show BizError where
  show = showError
instance Eq BizError where
  (==) = eqError

type ThrowsError = Either BizError

type IOThrowsError = ExceptT BizError IO

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left  err) = throwError err
liftThrows (Right val) = return val

data VarAttribute = Reliable
                  | NoDignity
                  deriving (Eq, Show)

data ErrorInfo = ErrorInfo String SourcePos