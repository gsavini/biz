module Biz.Data ( BizVal (..)
                , BizError (..)
                , Env
                , StashEnvs
                , ThrowsError
                , IOThrowsError
                , extractValue
                , liftThrows
                , VarAttribute (..)
                , Mut (..)
                ) where

import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad.Except (ExceptT, throwError)
import System.IO ( Handle )
import Data.IORef (IORef)

import qualified Data.Map as M
import qualified Data.List as L

data Mut = Mut { mName :: String
               , mValue :: IORef BizVal
               , attributes :: IORef [VarAttribute]
               , mPart :: String
               }

type Env = IORef [Mut]
type StashEnvs = IORef [(String, Env)]

data BizVal = Atom String
            | Number Double
            | String String
            | Bool Bool
            | FuncCall BizVal [BizVal]
            | If (BizVal) (Maybe BizVal) (Maybe BizVal) (Maybe BizVal)
            | Var BizVal BizVal [VarAttribute]
            | Seq [BizVal]
            | PrimitiveFunc ([BizVal] -> ThrowsError BizVal)
            | Funcf { func :: BizVal, paramss :: [BizVal], body :: BizVal }
            | Func {params :: [String], body :: BizVal, closure :: Env , stashEnvs :: StashEnvs}
            | IOFunc ([BizVal] -> IOThrowsError BizVal)
            | Port Handle
            | Arrivederci BizVal
            | DoppioLoop BizVal BizVal BizVal
            | InfiniteLoop BizVal
            | ForLoop BizVal BizVal BizVal BizVal BizVal
            | Mista BizVal
            | KingCrimson
            | Doppio [BizVal]
            | Dururu Integer (Maybe BizVal) BizVal
            | Rurudu Integer (Maybe BizVal) BizVal
            | Part String
            | SkipPart String (Maybe String)
            | User String (M.Map String BizVal)
            | Stand { name::String, abilities::(M.Map String BizVal) }
            | Ability { name::String, value::BizVal }
            | SingleArrow BizVal
            | Arrow BizVal BizVal
            | Cry [BizVal]
            | HolHorse [BizVal] BizVal
            | KillerQueen BizVal
            | BitesTheDust BizVal
            | TheHand BizVal
            | None
            | MoodyBlues Integer
            | GirlsDontLikeGuys BizVal BizVal [BizVal]
            | GirlItem BizVal BizVal
            | PorcaMiseria String BizVal
            | Cazzo BizVal
            | Crepa BizError

showVal :: BizVal -> String
showVal (Number n) = if n == fromInteger (round n) then (show . round $ n) else show n
showVal (String s) = "\"" ++ s ++ "\""
showVal (Bool True) = "yes"
showVal (Bool False) = "no"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, body = body, closure = env, stashEnvs = stashEnvs}) = "func "
showVal (IOFunc _) = "<IO primitive>"
showVal (Arrivederci val) = "arrivederci " ++ show val
showVal (DoppioLoop _ _ _) = "<list loop>"
showVal (InfiniteLoop _) = "<infinite loop"
showVal (ForLoop _ _ _ _ _) = "<for loop>"
showVal (Mista bv) = "<mista " ++ show bv ++ ">"
showVal KingCrimson = "King Crimson"
showVal (Doppio list) = "dop " ++ (unwords . map showVal $ list) ++ " pio"
showVal (Seq _) = "<seq>"
showVal (Stand name abilities) = "Stand " ++ name ++ " " ++ (L.intercalate ", " $ Prelude.map printPair $ M.toList abilities)
showVal (User user stands) = "User " ++ user ++ " " ++ (L.intercalate "; " $ Prelude.map (show . snd) $ M.toList stands)
showVal (Cry list) = concatMap ((++) " !" . show) list
showVal (Arrow first second) = show first ++ " -> " ++ show second
showVal (SingleArrow target) = "-> " ++ show target
showVal (Ability name value) = "Ability " ++ name ++ " " ++ show value
showVal (HolHorse _ _) = "holhorse"
showVal (KillerQueen name) = "Killer Queen: " ++ showVal name
showVal (Funcf _ _ _) = "func def"
showVal (MoodyBlues n) = "Moody Blues " ++ show n
showVal (Part name) = "Part " ++ name
showVal _ = "none"

printPair p = fst p ++ " " ++ (show $ snd p)

eq :: BizVal -> BizVal -> Bool
eq (Number n1) (Number n2) = n1 == n2
eq (String s1) (String s2) = s1 == s2
eq (Bool b1) (Bool b2) = b1 == b2
eq (Doppio d1) (Doppio d2) = d1 == d2
eq (User n1 s1) (User n2 s2) = n1 == n2 && s1 == s2
eq (Stand n1 a1) (Stand n2 a2) = n1 == n2 && a1 == a2
eq (Ability n1 bv1) (Ability n2 bv2) = n1 == n2 && bv1 == bv2
eq (Crepa be1) (Crepa be2) = be1 == be2
eq _ _ = False

instance Show BizVal where show = showVal
instance Eq BizVal where (==) = eq

unwordsList :: [BizVal] -> String
unwordsList = unwords . map showVal

data BizError = Parser ParseError
              | NumArgs Integer Integer
              | TypeMismatch String BizVal
              | UnboundName String
              | WrongArrow BizVal BizVal
              | FieldDoesntExist String String
              | ErrorCry BizVal BizVal
              | ReliableName String
              | NoneIsUnbindable
              | BitesTheNothing String
              | PartDoesntExist String
              | MissingPartDeclaration
              | FilePartDontMatch String String
              | SameNameDifferentParts String
              | WrongBizFile String
              | CazzoWithoutCrepa BizVal
              | CrepaWithoutPeli BizVal
              | GirlsLikePredicates BizVal
              | CustomError String (Maybe BizVal)

showError :: BizError -> String
showError (Parser err) = show err
showError (NumArgs expected found) = "Expected " ++ show expected ++ " arguments. Found " ++ show found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ". Found " ++ show found
showError (UnboundName name)  = "The name " ++ show name ++ " is unbound"
showError (WrongArrow bv1 bv2) = "Wrong arrow. You can't use it with " ++ show bv1 ++ " and "++ show bv2
showError (FieldDoesntExist value field) = show value ++ "\" doesn't have a field with the name \"" ++ field ++ "\""
showError (ErrorCry bv1 bv2) = "Wrong cry. You can't use it with" ++ show bv1 ++ " and " ++ show bv2
showError (ReliableName var) = "The name \"" ++ var ++ "\" is reliable. You can't update it"
showError NoneIsUnbindable = "none is unbindable"

showError (BitesTheNothing name) = "Bites the dust can't be activated because \"" ++ name ++ "\" doesn't exist"
showError (PartDoesntExist part) = "Part \"" ++ part ++ "\" doesn't exist"
showError (FilePartDontMatch fileName part) = "Filename and part name don't match. Filename: " ++ fileName ++ ". Part: " ++ part
showError (SameNameDifferentParts name) = "The name \"" ++ name ++ "\" is bond in different parts"
showError (WrongBizFile filename) = "File \"" ++ filename ++ "\" isn't a Biz file"
showError (CazzoWithoutCrepa notCrepa) = "Every cazzo needs a crepa. " ++ show notCrepa ++ " was provided"
showError (CrepaWithoutPeli crepa) = "Crepa " ++ show crepa ++ "without peli"
showError (GirlsLikePredicates notPred) = "Girls like guys who use predicates. \"" ++ show notPred ++ "\" is provided"
showError (CustomError name maybeData) = name ++ maybePrint maybeData
  where maybePrint a =
          case a of
            Nothing -> ""
            Just v -> ": " ++ show v
  
eqError :: BizError -> BizError -> Bool
eqError (CustomError name1 _) (CustomError name2 _) = name1 == name2
eqError _ _ = True

instance Show BizError where show = showError
instance Eq BizError where (==) = eqError

type ThrowsError = Either BizError

type IOThrowsError = ExceptT BizError IO

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

data VarAttribute = Reliable
                  | NoDignity
                  deriving (Eq, Show)
