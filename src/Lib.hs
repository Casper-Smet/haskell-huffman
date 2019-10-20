module Lib
    ( someFunc
    , countValues
    ) where

import Data.List (group, sort, sortOn, sortBy)
import Data.Ord (comparing)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data HuffmanTree = Node {character :: Char, weight :: Int} | Branch {left :: HuffmanTree, right :: HuffmanTree, weight :: Int} deriving (Show)

-- Adapted from https://stackoverflow.com/a/3711137
countValues :: String -> [(Char, Int)]
-- TODO: remove sortOn
countValues xss = sortOn snd $ map (\xs@(x:_) -> (x, length xs)) $ group $ sort xss

sortHuffman :: [HuffmanTree] -> [HuffmanTree]
sortHuffman = sortOn weight

createNodes :: [(Char, Int)] -> [HuffmanTree]
createNodes xs = sortHuffman [uncurry Node x | x <- xs]

mergeNodes t1 t2 = Branch t1 t2 $ weight t1 + weight t2

createTree :: [HuffmanTree] -> HuffmanTree
createTree (t1:t2:[]) = mergeNodes t1 t2
createTree (t1:t2:ts) = createTree $ sortHuffman $ mergeNodes t1 t2 : ts


test x = createTree $ createNodes $ countValues x

