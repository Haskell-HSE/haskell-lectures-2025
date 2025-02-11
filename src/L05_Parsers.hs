{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}



module L05_Parsers where
import Data.Char (isNumber)
import Control.Monad (ap, MonadPlus (..))
import Control.Applicative (Alternative (..))

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
  deriving (Functor)

instance Applicative Parser where
  pure x = Parser \s -> Just (x, s)
  (<*>) = ap

-- Void

instance Alternative Parser where
  empty = failure
  -- empty <|> p = p
  -- p <|> empty = p
  Parser p <|> Parser q = Parser \s ->
    case (p s, q s) of
      (Just r, _) -> Just r
      (_, Just r) -> Just r
      (Nothing, Nothing) -> Nothing

-- parser-combinators
-- parsec, attoparsec, megaparsec, ...

instance Monad Parser where
  Parser p >>= f = Parser \s0 -> case p s0 of
    Just (x, s1) -> runParser (f x) s1
    Nothing -> Nothing

instance MonadPlus Parser

peek :: Parser (Maybe Char)
-- | Returns first char in stream
peek = Parser \case
  "" -> Just (Nothing, "")
  s@(c:_) -> Just (Just c, s)

next :: Parser Char
next = Parser \case
  "" -> Nothing
  (c:cs) -> Just (c, cs)

failure :: Parser a
failure = Parser (const Nothing)

token :: (Char -> Bool) -> Parser Char
token p = do
  c <- next
  if p c
  then pure c
  else failure

parseInt :: Parser Int
-- | parseInt "12345"
--             ^
--             1^
--             12^
--             123^
--             1234^
--             12345^
--             12345
parseInt = impl 0
  where
    impl :: Int -> Parser Int
    -- impl acc = Parser \case
    --   "" -> (acc, "")
    --   s@(c:cs)
    --     | isNumber c -> runParser (impl (acc * 10 + fromEnum c - fromEnum '0')) cs
    --     | otherwise -> (acc, s)
    impl acc = do
      c0 <- peek
      case c0 of
        Nothing -> return acc
        Just c
          | isNumber c -> do
              _ <- next
              impl (acc * 10 + fromEnum c - fromEnum '0')
          | otherwise -> return acc

space :: Parser ()
space = token (== ' ') >> return ()

parsePairOfInts :: Parser (Int, Int)
-- parsePairOfInts "1234 124324" = ((1234, 124324), "")
-- parsePairOfInts = Parser \s ->
--   let (n, s') = runParser parseInt s
--    in case s' of
--       ' ':s'' -> let (m, s''') = runParser parseInt s'' in ((n, m), s''')
--       _ -> error "Parse error!!!"
-- parsePairOfInts =
--   parseInt >>= \n ->
--     token (== ' ') >>= \c ->
--       parseInt >>= \m ->
--         return (n, m)
parsePairOfInts = do
  n <- parseInt
  space
  m <- parseInt
  return (n, m)

parseTripleOfInts :: Parser (Int, Int, Int)
-- parseTripleOfInts "1234 12321323 2345 tail" = ((1234, 12321323, 2345), " tail")
parseTripleOfInts = do
  n <- parseInt
  space
  m <- parseInt
  space
  k <- parseInt
  return (n, m, k)

try :: Parser a -> Parser (Maybe a)
try p = (p >>= \x -> return (Just x)) <|> return Nothing

parseListOfInts :: Parser [Int]
-- parseListOfInts "1234 12345 2345345 54 tail" = ([1234, 12345, 2345345, 54], "tail")
-- parseListOfInts "1234 1231tail" -- parse error!!!
parseListOfInts = do
  n0 <- try (do {n <- parseInt; space; return n})
  case n0 of
    Just n -> do
      ns <- parseListOfInts
      return (n:ns)
    Nothing -> return []

main :: IO ()
main = do
  line <- getLine
  print (runParser parseListOfInts line)
