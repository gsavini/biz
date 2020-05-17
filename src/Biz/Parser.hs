module Biz.Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Char
import           Control.Monad (liftM, void)
import           Data.IORef (IORef)
import           Control.Monad.Except (ExceptT)
import           System.IO (Handle)
import           Numeric (readFloat)
import           Data.Maybe (isJust)
import qualified Data.Map as M
import           Text.Parsec.Language
import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Tokens

import           Data.Char (isSpace)
import           Data.Map as M

import           Biz.Data

symbol :: Parser Char
symbol = oneOf "+-*/|><=&%$#"

spaces1 :: Parser ()
spaces1 = skipMany1 space

text :: Parser BizVal
text = do
  char '"'
  x <- many $ parseEscapedChars <|> (noneOf "\"")
  char '"'
  return $ String x

parseEscapedChars :: Parser Char
parseEscapedChars = do
  char '\\'
  c <- oneOf "\\\"ntr"
  return $ case c of
    '\\' -> c
    '\"' -> c
    'n' -> '\n'
    't' -> '\t'
    'r' -> '\r'

atom :: Parser BizVal
atom = do
  a <- many1 $ satisfy sat
  return $ Atom a

sat :: Char -> Bool
sat c = not $ isSpace c || c `elem` ":(){}!,"

number :: Parser BizVal
number = do
  maybeMinus <- optionMaybe $ char '-'
  num <- many1 digit
  case maybeMinus of
    Just _ -> return $ Number $ read $ '-' : num
    Nothing -> return $ Number $ read $ num

float :: Parser BizVal
float = do
  maybeMinus <- optionMaybe $ char '-'
  x <- many1 digit
  char '.'
  y <- many1 digit
  case maybeMinus of
    Just _ -> return $ Number $ read ("-" ++ x ++ "." ++ y)
    Nothing -> return $ Number $ read (x ++ "." ++ y)

yes :: Parser BizVal
yes = do
  many1 $ string "yes"
  notFollowedBy alphaNum
  return $ Bool True

no :: Parser BizVal
no = do
  many1 $ string "no"
  notFollowedBy alphaNum
  return $ Bool False

funcCall :: Parser BizVal
funcCall = do
  string "oingo"
  spaces1
  func:args <- endBy (notFollowedBy (string "jo") >> expr) $ try spaces
  string "jo"
  return $ FuncCall func args

opCall :: Parser BizVal
opCall = do
  string "oingo"
  spaces1
  arg1 <- expr
  spaces1
  op <- atom
  spaces1
  arg2 <- expr
  return $ FuncCall op [arg1, arg2]

conditional :: Parser BizVal
conditional = do
  try (string "which fist") <|> string "will i hit you with my right fist or my left"
  spaces1
  predicate <- expr
  maybeRight <- optionMaybe (try $ spaces1 >> right)
  maybeLeft <- optionMaybe (try $ spaces1 >> left)
  maybeBoth <- optionMaybe (try $ spaces1 >> both)
  return $ If predicate maybeLeft maybeRight maybeBoth

left :: Parser BizVal
left = do
  string "left"
  spaces1
  leftBranch <- expr
  return leftBranch

right :: Parser BizVal
right = do
  string "right"
  spaces1
  rightBranch <- expr
  return rightBranch

both :: Parser BizVal
both = do
  string "both"
  spaces1
  bothBranch <- expr
  return bothBranch

var :: Parser BizVal
var = do
  string "kono"
  spaces1
  reliable <- optionMaybe (try $ string "reliable" >> notFollowedBy alphaNum >> return "reliable")
  noDignity <- optionMaybe (try $ string "nodignity" >> notFollowedBy alphaNum >> return "nodignity")
  spaces
  name <- atom 
  spaces1
  value <- topLevel
  spaces1
  string "da"
  let attributes = computeAttr [reliable, noDignity]
  return $ Var name value attributes

computeAttr :: [Maybe String] -> [VarAttribute]
computeAttr [] = []
computeAttr (x:xs) =
  case x of
    Nothing -> computeAttr xs
    Just attrDesc -> (getAttr attrDesc) : (computeAttr xs)

getAttr :: String -> VarAttribute
getAttr attr
  | attr == "reliable" = Reliable
  | attr == "nodignity" = NoDignity
  
streak :: Parser BizVal
streak = do
  exprs <- between (char '{' >> spaces)
           (spaces >> char '}')
           (sepEndBy (spaces >>  notFollowedBy (char '}') >> topLevel) (many1 eeol >> spaces))
  return $ Seq exprs

eeol :: Parser Char
eeol = (endOfLine <|> newline)
  
funcDef :: Parser BizVal
funcDef = do
  string "boingo"
  spaces1
  name:params <- sepEndBy atom $ spaces
  string ":"
  spaces
  body <- expr
  return $ Funcf name params body

arrivederci :: Parser BizVal
arrivederci =
  try (do string "arrivederci"
          spaces1
          expression <- expr 
          return $ Arrivederci expression
      )
  <|>
  (do many $ try $ string "ari"
      string "arri"
      spaces1
      expression <- expr 
      spaces1
      string "vederci"
      return $ Arrivederci expression
  )

forLoop :: Parser BizVal
forLoop = do
  string "gold"
  spaces1
  localVar <- atom
  spaces1
  startingValue <- parensExpr
  spaces1
  string "experience"
  spaces1
  increase <- parensExpr
  spaces1
  string "requiem"
  spaces1
  predicate <- parensExpr
  spaces1
  body <- parensExpr
  return $ ForLoop localVar startingValue increase predicate body

infiniteLoop :: Parser BizVal
infiniteLoop  = do
  try $ string "ger" <|> string "goldexperiencerequiem"
  spaces1
  body <- parensExpr
  return $ InfiniteLoop body

doppioLoop :: Parser BizVal
doppioLoop  = do
  try $ string "ger" <|> string "goldexperiencerequiem"
  spaces1
  localVar <- atom
  spaces1
  bv <- expr
  spaces1
  body <- expr
  return $ DoppioLoop localVar bv body

mista :: Parser BizVal
mista  = do
  string "mista"
  spaces1
  expression <- expr 
  return $ Mista expression

kingCrimson :: Parser BizVal
kingCrimson  = do
  ((string "king" <|> string "emperor") >> spaces >> string "crimson")
    <|> string "i erased time and leapt past it"
  return $ KingCrimson
  
doppio :: Parser BizVal
doppio =
  (do try $ string "doppio"
      return $ Doppio []) <|>
  (do
      string "dop"
      spaces1
      values <- endBy ((try $ notFollowedBy $ string "pio") >> expr) $ try spaces1
      string "pio"
      return $ Doppio values)

dururu :: Parser BizVal
dururu  = do
  string "du"
  rus <- many $ string "ru"
  spaces1
  maybeMoshi <- optionMaybe $ moshimoshi
  doppio <- expr 
  return $ Dururu (flip (-) 1. toInteger . length $ rus) (maybeMoshi) doppio

rurudu :: Parser BizVal
rurudu  = do
  rus <- many $ string "ru"
  string "du"
  spaces1
  maybeMoshi <- optionMaybe $ moshimoshi
  doppio <- expr 
  return $ Rurudu (flip (-) 1. toInteger . length $ rus) (maybeMoshi) doppio

moshimoshi :: Parser BizVal
moshimoshi = do
  moshi <- string "moshimoshi"
  spaces1
  e <- expr
  spaces1
  return e

part :: Parser BizVal
part = do
  string "part"
  spaces1
  first <- letter
  rest <- many1 (letter <|> char '.')
  return $ Part (first:rest)

skipPart :: Parser BizVal
skipPart = do
  string "skip part"
  spaces1
  first <- letter
  rest <- many1 (letter <|> char '.')
  return $ SkipPart (first:rest) Nothing

skipPartAs :: Parser BizVal
skipPartAs = do
  string "skip part"
  spaces1
  first <- letter
  rest <- many1 (letter <|> char '.')
  spaces1
  string "as"
  spaces1
  name <- many1 letter
  return $ SkipPart (first:rest) (Just name)

user :: Parser BizVal
user = do
  string "user"
  spaces
  userName <- many1 letter
  return $ User userName M.empty

stand :: Parser BizVal
stand  = do
  string "stand"
  spaces1
  name <- many1 letter
  maybeAbilities <- optionMaybe $ try $ do
    spaces
    abilities <- between (char ':') (spaces >> char ':') (sepBy (spaces >> standAbility) (try $ spaces >> char ';'))
    return abilities
  case maybeAbilities of
    Nothing -> return $ Stand name (M.empty)
    Just abilities -> return $ Stand name (fromList abilities)

standAbility :: Parser (String, BizVal)
standAbility = do
  string "ability"
  spaces
  name <- many1 letter
  spaces1
  value <- topLevel
  return $ (name, value)

arrowSingle :: Parser BizVal
arrowSingle  = do
  string "->"
  spaces
  userOrStand <- expr 
  return $ SingleArrow userOrStand

ability :: Parser BizVal
ability = do
  string "ability"
  spaces
  name <- many1 letter
  spaces1
  value <- topLevel
  return $ Ability name value

parens = between (char '(' >> spaces) (spaces >> char ')') --Tokens.parens haskell
identifier = Tokens.identifier haskell
reserved = Tokens.reservedOp haskell

infixLeft operator func =
  Expr.Infix (spaces >> reserved operator >> return func) Expr.AssocLeft

infixRight operator func =
  Expr.Infix (spaces >> reserved operator >> spaces >> return func) Expr.AssocRight

infixRightArrow =
  Expr.buildExpressionParser table (notFollowedBy newline >> expr)
  where
    table = [[infixLeft "->" Arrow]]

infixLeftArrow =
  Expr.buildExpressionParser table (expr)
  where
    table = [[infixRight "<-" (flip Arrow)]]


cry :: Parser BizVal
cry = do
  items <- endBy1 expr (try (many1 (char '!')))
  return $ Cry items

freccia :: Parser (BizVal -> BizVal -> BizVal)
freccia = do
  string "->"
  return Arrow

holHorse :: Parser BizVal
holHorse = do
  string "hol"
  spaces
  string "horse"
  spaces1
  params <- sepEndBy atom $ spaces
  char ':'
  spaces
  body <- try $ expr 
  return $ HolHorse params body

killerQueen :: Parser BizVal
killerQueen = do
  string "killer"
  spaces1
  string "queen"
  spaces1
  a <- atom
  return $ KillerQueen a

bitesTheDust :: Parser BizVal
bitesTheDust = do
  string "bites"
  spaces1
  (string "the" <|> string "za")
  spaces1
  (string "dusto" <|> string "dust")
  spaces1
  a <- atom
  return $ BitesTheDust a

theHand :: Parser BizVal
theHand = do
  (string "za" >> spaces1 >> string "hando") <|> (string "the" >> spaces1 >> string "hand")
  spaces1
  a <- atom
  return $ TheHand a

moodyBlues :: Parser BizVal
moodyBlues = do
  string "moody"
  spaces
  string "blues"
  spaces1
  num <- many1 digit
  return $ MoodyBlues (read num)

girlsDontLikeGuys :: Parser BizVal
girlsDontLikeGuys = do
  string "listen"
  spaces1
  val <- expr
  spaces
  string ", girls don't like"
  spaces1
  predicate <- expr
  spaces1
  string "who"
  spaces
  items <- between (char '{' >> spaces)
           (spaces >> char '}')
           (sepEndBy (spaces >>  notFollowedBy (char '}') >> girlsItem) (many1 eeol >> spaces))
  return $ GirlsDontLikeGuys val predicate items

girlsItem :: Parser BizVal
girlsItem = do
  val <- expr
  spaces
  char ':'
  spaces
  result <- expr
  return $ GirlItem val result

porcaMiseria :: Parser BizVal
porcaMiseria = do
  string "porca"
  spaces
  string "miseria"
  spaces1
  errorOrigin <- many1 $ satisfy sat
  spaces1
  bizVal <- expr
  return $ PorcaMiseria errorOrigin bizVal

cazzo :: Parser BizVal
cazzo = do
  string "cazzo"
  spaces1
  error <- expr
  return $ Cazzo error

crepa :: Parser BizVal
crepa = do
  string "crepa"
  spaces1
  errorName <- many1 $ satisfy sat
  maybeData <- optionMaybe $ try (spaces1 >> expr)
  return $ Crepa $ CustomError errorName maybeData
  
topLevel :: Parser BizVal
topLevel =
  try cry
  <|> try infixRightArrow
  <|> try infixLeftArrow
  <|> try (parens expr)
  <|> expr

parensExpr :: Parser BizVal
parensExpr = try (parens expr) <|> expr

expr :: Parser BizVal
expr =
  try (parens infixLeftArrow)
  <|> try (parens infixRightArrow)
  <|> try (parens cry)
  <|> text
  <|> try float
  <|> try number
  <|> try yes
  <|> try no
  <|> streak
  <|> try conditional
  <|> try funcDef
  <|> try funcCall
  <|> try holHorse
  <|> try opCall
  <|> try doppioLoop
  <|> try infiniteLoop
  <|> try forLoop
  <|> try arrivederci
  -- <|> try mista
  <|> try kingCrimson
  <|> try doppio
  <|> try dururu
  <|> try rurudu
  <|> try part
  <|> try skipPartAs
  <|> try skipPart
  <|> try user
  <|> try stand
  <|> try ability
  <|> try killerQueen
  <|> try bitesTheDust
  <|> try theHand
  <|> try moodyBlues
  <|> try girlsDontLikeGuys
  <|> try porcaMiseria
  <|> try cazzo
  <|> try crepa
  <|> try var
  <|> atom
