module Main where

import Control.Monad (forever)
import Lib (execute)

main :: IO ()
main = forever $ fmap execute getLine >>= putStrLn
