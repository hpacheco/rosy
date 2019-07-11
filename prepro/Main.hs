module Main where

import System.Environment
import Rosy.PreProcessor

main = do
    args <- getArgs
    let from = args!!1
    let to = args!!2
    preprocessor from to