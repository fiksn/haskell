{- Mul - inspired by AOC 2024 Day 3 Gregor Pogacnik -}

import Data.Char (isDigit, toLower)
import Control.Applicative
import Data.Maybe (fromMaybe)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- Functor instance for Parser
instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> case p input of
        Just (result, rest) -> Just (f result, rest)
        Nothing -> Nothing

-- Applicative instance for Parser
instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    (Parser pf) <*> (Parser p) = Parser $ \input -> case pf input of
        Just (f, rest) -> case p rest of
            Just (result, finalRest) -> Just (f result, finalRest)
            Nothing -> Nothing
        Nothing -> Nothing

-- Alternative instance for backtracking
instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

-- Monad instance for sequencing
instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \input -> case p input of
        Just (result, rest) -> runParser (f result) rest
        Nothing -> Nothing

-- Primitive parsers
char :: Char -> Parser Char
char c = Parser $ \input -> case input of
    (x:xs) | x == c -> Just (x, xs)
    _ -> Nothing

caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = satisfy (\x -> toLower x == toLower c)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \input -> case input of
    (x:xs) | f x -> Just (x, xs)
    _ -> Nothing

string :: String -> Parser String
string = traverse char

stringCI :: String -> Parser String
stringCI = traverse caseInsensitiveChar

retry :: Parser a -> Parser a
retry p = Parser $ \input -> case runParser p input of
    Just result -> Just result
    Nothing -> case input of
        (_:xs) -> runParser (retry p) xs
        [] -> Nothing

-- Parses an integer (1 to 3 digits)
parseSmallInt :: Parser Int
parseSmallInt = do
    digits <- some (satisfy isDigit)
    let n = read digits
    if n >= 1 && n <= 999
        then return n
        else empty

-- Parses a single `mul(...)` occurrence
parseMulCore :: Parser ([Int])
parseMulCore = do
    _ <- stringCI "mul"
    _ <- char '('
    first <- parseSmallInt
    _ <- char ','
    second <- parseSmallInt
    _ <- char ')'
    return ([first, second])

parseAllMuls :: Parser [[Int]]
parseAllMuls = many $ retry parseMulCore

-- Utilities
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

main :: IO ()
main = do
  input <- getContents
  let result = do
        r <- runParser parseAllMuls input
        return (sum $ fmap product (fst r))
  case result of
    Nothing -> putStrLn "No solution"
    Just x -> putStrLn $ show x
