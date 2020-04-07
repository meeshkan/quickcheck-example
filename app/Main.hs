module Main where

import Data.Either
import Lib

main :: IO ()
main = putStrLn $ either show dumpJSON (parseJSON "{\"a\"   : 1 }")
