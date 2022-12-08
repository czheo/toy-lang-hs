module Parser where
data Expr = Number Double
          | Add Expr
          | Sub Expr
          | Mul Expr
          | Div Expr
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

src = Src {line = 0, col = 0, text = undefined}

charP :: Char -> Parser Char
charP ch = Parser f
  where f src@Src{text = c:rem} | c == ch = Just (c, src')
          where src' = case c of
                       '\n' -> src {line = line src + 1, col = 0, text = rem}
                       _ -> src {col = col src + 1, text = rem}
        f _ = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

parse :: String -> Maybe Expr
parse text = do
  (num, _) <- runP (read <$> stringP "123") src {text = text}
  return $ Number num
