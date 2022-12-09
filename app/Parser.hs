module Parser where
import Control.Applicative
import Data.Char

data Expr = Number Double
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Boolean Bool
          deriving Show

data Src =
  Src { line :: Int
      , col :: Int
      , text :: String
  } deriving Show

newtype Parser a = Parser {
  runP :: Src -> Maybe (a, Src)
}

instance Functor Parser where
  fmap f (Parser p)= Parser _p'
    where _p' src = do
                    (a, src') <- p src
                    return (f a, src')

instance Applicative Parser where
  pure a = Parser (\src -> Just (a, src))
  Parser p1 <*> Parser p2 = Parser p3
    where p3 src = do
                   (f, src') <- p1 src
                   (a, src'') <- p2 src'
                   return (f a, src'')

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser f
    where f src = p1 src <|> p2 src

instance Monad Parser where
  Parser p >>= f = Parser p'
    where p' src = case p src of
                   Nothing -> Nothing
                   Just (a, src) -> runP (f a) src

src = Src {line = 0, col = 0, text = undefined}

ifCharP :: (Char -> Bool) -> Parser Char
ifCharP predicate = Parser f
  where f src@Src{text = c:rem} = if predicate c then Just (c, src') else Nothing
          where src' = case c of
                       '\n' -> src {line = line src + 1, col = 0, text = rem}
                       _ -> src {col = col src + 1, text = rem}
        f _ = Nothing

charP :: Char -> Parser Char
charP c = ifCharP (==c)

stringP :: String -> Parser String
stringP = sequenceA . map charP

keywordP :: String -> Expr -> Parser Expr
keywordP s exp = const exp <$> stringP s

spanP :: (Char -> Bool) -> Parser String
spanP pred = some $ ifCharP pred

boolExpr :: Parser Expr
boolExpr = trueExpr <|> falseExpr
    where trueExpr = keywordP "true" $ Boolean True
          falseExpr = keywordP "false" $ Boolean False

numberExpr :: Parser Expr
numberExpr = (\s -> Number $ read s) <$> spanP isDigit

arithExpr :: Parser Expr
arithExpr = do
  n1 <- numberExpr
  many $ charP ' '
  op <- ifCharP (\c -> c `elem` "+-*/")
  many $ charP ' '
  n2 <- numberExpr
  return $ (toOp op) n1 n2
  where toOp '+' = Add
        toOp '-' = Sub
        toOp '*' = Mul
        toOp '/' = Div

exprP = arithExpr <|> numberExpr <|> boolExpr

parse :: String -> Maybe Expr
parse text = do
  (expr, _) <- runP exprP src {text = text}
  return expr
