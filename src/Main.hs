module Main where
import           Control.Monad                 hiding (sequence)
import           Text.ParserCombinators.Parsec
import Data.List
import System.Environment (getArgs)
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
        Right program -> do
                        args <- getArgs
                        case args of
                            ["filter", nucleobases] -> do
                                                        filtered <- seqfilter (parse (many1 nucleobase) "" nucleobases) program

                                                        print filtered
                            ["stats"] -> print (seqcount program)
                            [] -> print program








seqcount :: [Sequence] -> Int
seqcount = length

seqfilter :: [Nucleobase] -> [Sequence] -> [Sequence]
seqfilter toKeep = filter f
            where f (Sequence _ bases) = toKeep `isInfixOf` map key bases

main :: IO ()
main = process =<< getContents
