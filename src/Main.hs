module Main where

data Sequence = Sequence { id :: String, bases :: [Base] }
                deriving (Show)
                
data Base = Base { key :: Nucleobase, score :: Float }
                deriving (Eq, Show)


data Nucleobase = A | C | G | T | U | N
                deriving (Eq, Show, Read)



main :: IO ()
main = do
  putStrLn "hello world"
