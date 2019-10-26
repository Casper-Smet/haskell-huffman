module Lib
    ( countValues
    , createNodes
    , createTree
    ) where

import Data.List (group, sort)


data HuffmanTree = Node {character :: Char, weight :: Int} | Branch {left :: HuffmanTree, right :: HuffmanTree, weight :: Int} deriving (Show, Eq)

instance Ord HuffmanTree where
    compare x y = compare (weight x) (weight y)

-- Adapted from https://stackoverflow.com/a/3711137
countValues :: String -> [(Char, Int)]
countValues = map (\xs@(x:_) -> (x, length xs)) . group . sort

createNodes :: [(Char, Int)] -> [HuffmanTree]
createNodes xs = sort [uncurry Node x | x <- xs]

mergeNodes t1 t2 = Branch t1 t2 $ weight t1 + weight t2

createTree :: [HuffmanTree] -> HuffmanTree
createTree [t1, t2] = mergeNodes t1 t2
createTree (t1:t2:ts) = createTree $ sort $ mergeNodes t1 t2 : ts

test x = createTree $ createNodes $ countValues x

