module Main where
import           Control.Monad                 hiding (sequence)
import           System.Environment            (getArgs)
import           Text.ParserCombinators.Parsec

data Sequence = Sequence { id :: String, bases :: [Base] }
                deriving (Show)

data Base = Base { key :: Nucleobase, quality :: Char }
                deriving (Eq, Show)

data Nucleobase = A | C | G | T | U | N
                deriving (Eq, Show, Read)

nucleobase :: Parser Nucleobase
nucleobase = msum [ t <$ char c | (c, t) <- types ] <|> N <$ noneOf "\n"
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
            newline
            raw <- many1 nucleobase
            newline
            char '+'
            skipMany (noneOf "\n")
            newline
            qualities <- many1 (noneOf "\n")
            newline
            return $ Sequence id (zipWith Base raw qualities)

parseSeq :: String -> Either ParseError [Sequence]
parseSeq = parse (many1 Main.sequence) ""


process :: String -> IO ()
process contents = case parseSeq contents of
                        Left err -> print err
                        Right program -> print program

seqcount :: [Sequence] -> Int
seqcount = length


main :: IO ()
main = process =<< getContents
