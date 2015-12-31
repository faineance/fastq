module Main where
import           Control.Monad                 hiding (sequence)
import           System.Environment            (getArgs)
import           Text.ParserCombinators.Parsec

data Sequence = Sequence { id :: String, bases :: [Base] }
                deriving (Show)

data Base = Base { key :: Nucleobase, score :: Char }
                deriving (Eq, Show)

data Nucleobase = A | C | G | T | U | N
                deriving (Eq, Show, Read)

nucleobase :: Parser Nucleobase
nucleobase = msum [ t <$ char c | (c, t) <- types ] <|> N <$ noneOf "+"
     where types =
             [ ('A', A)
             , ('C', C)
             , ('G', G)
             , ('T', T)
             , ('U', U)]

sequence :: Parser Sequence
sequence = do
    _ <- char '@'
    id <- many1 (noneOf "\n")
    raw <- many1 nucleobase
    _ <- string "+\n"
    scores <- many1 (noneOf "\n")
    _ <- char '\n'
    return $ Sequence id (zipWith Base raw scores)


parseSeq :: String -> Either ParseError [Sequence]
parseSeq = parse (many1 Main.sequence) ""


process :: String -> IO ()
process contents = case parseSeq contents of
                        Left err -> print err
                        Right program -> print program

main :: IO ()
main = do
  [file] <- getArgs
  process =<< readFile file
