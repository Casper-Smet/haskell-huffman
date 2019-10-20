module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Enter String for Huffman coding"
    input <-  getLine
    print $ createTree $ createNodes $ countValues input
