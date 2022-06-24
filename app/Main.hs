module Main where

import LOptions (programLOptions)
import LSystem (startLSystem)
import Options.Applicative (execParser)

main :: IO ()
main = startLSystem =<< execParser programLOptions
