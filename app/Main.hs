module Main where

import LOptions (programLOptions)
import Options.Applicative (execParser)

main :: IO ()
main = putStrLn "Hello World !" << execParser programLOptions
