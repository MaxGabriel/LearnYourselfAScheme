import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Data.Char
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

binDigit :: Parser Char
binDigit = oneOf "01"


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ (noneOf "\"")
    char '"'
    return $ String x

--parseNumber :: Parser LispVal
--parseNumber = many1 digit >>= (return . Number . read)

-- #b (binary), #o (octal), #d (decimal), and #x (hexadecimal)

-- These functions are unsafe b/c they assume a succesful parse, btu 
--trueReadBin :: String -> Integer
--trueReadBin s = fst $ head $ readInt 2 (\_ -> True) digitToInt s

--trueReadHex :: String -> Integer
--trueReadHex s = fst $ head $ readHex s

--trueReadOct :: String -> Integer
--trueReadOct s = fst $ head $ readOct s

--parseNumber :: Parser LispVal
--parseNumber = do
--    radix <- string "#b" <|> string "#o" <|> string "#x" <|> string "#d" <|> string ""
--    case radix of
--      "#b" -> many1 binDigit >>= (return . Number. trueReadBin)
--      "#o" -> many1 octDigit >>= (return . Number . trueReadOct)
--      "#x" -> many1 hexDigit >>= (return . Number . trueReadHex)
--      "#d" -> many1 digit >>= (return . Number . read)
--      ""   -> many1 digit >>= (return . Number . read)

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseListOrDottedList :: Parser LispVal
parseListOrDottedList = do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x

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
        <|> parseListOrDottedList

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

-- Printing values

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- Evaling

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("boolean?", isBoolean),
              ("symbol?", isAtom),
              ("number?", isLispNumber)]

isBoolean :: [LispVal] -> LispVal
isBoolean [Bool _] = Bool True
isBoolean _ = Bool False

isAtom :: [LispVal] -> LispVal
isAtom [Atom _] = Bool True
isAtom _ = Bool False

isLispNumber :: [LispVal] -> LispVal
isLispNumber [Number _] = Bool True
isLispNumber _ = Bool False


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n ::[(Integer, String)] in
                          if null parsed
                            then 0
                            else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0


main :: IO ()
main = getArgs >>= print . eval . readExpr . head
