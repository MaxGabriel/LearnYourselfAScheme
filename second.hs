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

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))
