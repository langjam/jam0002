module Main where

import Parser
import qualified Data.Text.IO as T

main :: IO ()
main = do
    file <- T.readFile "example.pt"
    case parseProgram "example.pt" file of
        Left err -> putStrLn err
        Right res -> print res

