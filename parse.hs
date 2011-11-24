--parse.hs
module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where show = showVal

data LispError = NumArgs Int [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError

instance Error LispError where
  noMsg = Default "An error has occurred."
  strMsg = Default

type ThrowsError = Either LispError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ func
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " arg(s); found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escaped :: Char -> Char -> Parser Char
escaped s c = do
  char '\\' >> char s
  return c

validStringChar :: Parser Char
validStringChar = noneOf "\""
              <|> escaped '\\' '\\'
              <|> escaped 'n' '\n'
              <|> escaped 't' '\t'
              <|> escaped 'r' '\r'

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many validStringChar
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
  do result <- eval pred
     case result of
       Bool False -> eval alt
       otherwise -> eval conseq
eval (List (Atom fn : args)) = mapM eval args >>= apply fn
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply fn args = maybe (throwError $ NotFunction "Unrecognized primitive function args" fn)
                      ($ args)
                      (lookup fn primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", args2 $ numericBinop (+)),
              ("-", args2 $ numericBinop (-)),
              ("*", args2 $ numericBinop (*)),
              ("/", args2 $ numericBinop div),
              ("mod", args2 $ numericBinop mod),
              ("quotient", args2 $ numericBinop quot),
              ("remainder", args2 $ numericBinop rem),
              ("=", args2 $ numBoolBinop (==)),
              ("<", args2 $ numBoolBinop (<)),
              (">", args2 $ numBoolBinop (>)),
              ("<=", args2 $ numBoolBinop (<=)),
              (">=", args2 $ numBoolBinop (>=)),
              ("&&", args2 $ boolBoolBinop (&&)),
              ("||", args2 $ boolBoolBinop (||)),
              ("string=?", args2 $ strBoolBinop (==)),
              ("string<?", args2 $ strBoolBinop (<)),
              ("string>?", args2 $ strBoolBinop (>)),
              ("string<=?", args2 $ strBoolBinop (<=)),
              ("string>=?", args2 $ strBoolBinop (>=)),
              ("boolean?", args1 isBoolean),
              ("list?", args1 isList),
              ("pair?", args1 isPair),
              ("number?", args1 isNumber),
              ("symbol?", args1 isSymbol),
              ("string?", args1 isString)
              ]

args :: Int -> ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
args n fn val = if n /= length val
                  then throwError $ NumArgs n val
                  else fn val

args1 = args 1
args2 = args 2

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = do
  left <- unpacker $ args !! 0
  right <- unpacker $ args !! 1
  return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool
strBoolBinop = boolBinop unpackString

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                          if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackString :: LispVal -> ThrowsError String
unpackString (String s) = return s
unpackString (Number n) = return $ show n
unpackString (Bool b) = return $ show b
unpackString notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean [Bool _] = return $ Bool True
isBoolean _ = return $ Bool False

isList :: [LispVal] -> ThrowsError LispVal
isList [List _] = return $ Bool True
isList _ = return $ Bool False

isPair :: [LispVal] -> ThrowsError LispVal
isPair [List []] = return $ Bool False
isPair [List _] = return $ Bool True
isPair [DottedList _ _] = return $ Bool True
isPair _ = return $ Bool False

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number _] = return $ Bool True
isNumber _ = return $ Bool False

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol _ = return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString _ = return $ Bool False

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled