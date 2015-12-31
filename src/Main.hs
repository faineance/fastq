module Main where
import           Text.ParserCombinators.Parsec
import Control.Monad hiding (sequence)
import System.Environment (getArgs)

data Sequence = Sequence { id :: String, bases :: [Base] }
                deriving (Show)

data Base = Base { key :: Nucleobase, score :: Char }
                deriving (Eq, Show)


data Nucleobase = A | C | G | T | U | N
                deriving (Eq, Show, Read)

nucleobase :: Parser Nucleobase
nucleobase = msum [ t <$ char c | (c, t) <- types ] <|> N <$ anyChar
     where types =
             [ ('A', A)
             , ('C', C)
             , ('G', G)
             , ('T', T)
             , ('U', U)]


sequence :: Parser Sequence
sequence = do
    char '@'
    id <- many1 (noneOf "\n")
    raw <- many1 nucleobase
    char '+'
    scores <- many1 (noneOf "\n")
    return $ Sequence id (zipWith Base raw scores)

process :: String -> IO ()
process contents = return ()

main :: IO ()
main = do
  [file] <- getArgs
  process =<< readFile file
