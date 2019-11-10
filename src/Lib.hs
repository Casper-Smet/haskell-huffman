{-|
Module      : Lib
Description : Module containing all necessary functions to encode and decode a string using Huffman encoding
Copyright   : (c) Casper Smet, 2019
License     : BSD3
Maintainer  : casper.smet@student.hu.nl

This module contains all necessary functions to encode to and decode from Huffman codes. 
This module was created for the Utrecht University of Applied Sciences AI course 'Declarative Programming'


-}
module Lib
    ( countValues
    , createNodes
    , createTree
    , encodeTree
    , encodeString
    , decodeList
    , tupleToMap
    , codedString
    , mergeNodes
    , Binary(I, O)
    , HuffmanTree(Node, Branch)
    ) where

import Data.List (group, sort)
import Data.Map (Map, fromList, (!), member)

-- ##     ## ##     ## ######## ######## ##     ##    ###    ##    ## 
-- ##     ## ##     ## ##       ##       ###   ###   ## ##   ###   ## 
-- ##     ## ##     ## ##       ##       #### ####  ##   ##  ####  ## 
-- ######### ##     ## ######   ######   ## ### ## ##     ## ## ## ## 
-- ##     ## ##     ## ##       ##       ##     ## ######### ##  #### 
-- ##     ## ##     ## ##       ##       ##     ## ##     ## ##   ### 
-- ##     ##  #######  ##       ##       ##     ## ##     ## ##    ## 

-- | Binary datatype, uses I and O
data Binary 
            = I                                     -- ^ Binary 1
            | O                                     -- ^ Binary 0 
                deriving (Show, Ord, Eq)            
    
    
-- | HuffmanTree datatype, uses Node and Branch and branch constructors. HuffmanTrees are compared by weight.
data HuffmanTree 
    -- | The Node (alternatively, the Leaf) is a type of HuffmanTree containing a Char and a Weight.
    = Node {character :: Char, weight :: Int} 
    -- | The Branch is a type of HuffmanTree containting two other HuffmanTreess and the sum of the weight of these HuffmanTreess.
    | Branch {left :: HuffmanTree, right :: HuffmanTree, weight :: Int} deriving (Show, Eq)

instance Ord HuffmanTree where
    -- | HuffmanTree's are compared by weight.
    compare x y = compare (weight x) (weight y)

-- | The 'countValues' function counts each unique character in a string. Adapted from https://stackoverflow.com/a/3711137.
countValues :: String           -- ^ The text to be encoded
            -> [(Char, Int)]    -- ^ (Unique character, frequency of said character)
countValues = map (\xs@(x:_) -> (x, length xs)) . group . sort

-- | The 'createNodes' function creates a Node for each tuple. The frequency of a character equals its Weight.
createNodes :: [(Char, Int)]    -- ^ Unique character is text, frequency of character in text
            -> [HuffmanTree]    -- ^ List of Node
createNodes xs = sort [uncurry Node x | x <- xs]

-- | The 'mergeNodes' function merges two HuffmanTree\'s into one Branch. 
-- | The weight of the new Branch being the sum of the weight of it's predecessors.
mergeNodes  :: HuffmanTree      -- ^ HuffmanTree 1
            -> HuffmanTree      -- ^ HuffmanTree 2
            -> HuffmanTree      -- ^ Merged HuffmanTree 1 & 2
mergeNodes t1 t2 = Branch t1 t2 $ weight t1 + weight t2

-- | The 'createTree' function recursively creates one HuffmanTree from a list of HuffmanTrees. 
-- | It merges the two HuffmanTree with the lowest Weight, adds them to the list of HuffmanTrees and repeats until only one tree is left
createTree  :: [HuffmanTree]    -- ^ List of HuffmanTrees
            -> HuffmanTree      -- ^ Resulting HuffmanTree
createTree []           = error "createTree; Supplied empty list"
createTree [t]          = t 
createTree [t1, t2]     = mergeNodes t1 t2
createTree (t1:t2:ts)   = createTree $ sort $ mergeNodes t1 t2 : ts

-- | The 'encodeTree' function traverses (Depth First) a HuffmanTree. It then creates a code for each Node (or Leaf)
-- Each time it goes left, it adds O to the path, every time it goes right it adds I to the path. 
-- When it reaches a Node, it returns a tuple containing the character and the path (the path needs to be reversed to be traversable in decoding).
encodeTree  :: HuffmanTree          -- ^ Completed HuffmanTree
            -> [(Char, [Binary])]   -- ^ List of tuples containing character and path (binary code)
encodeTree t = encodeTree' t []
    where 
        encodeTree' (Node c _) x = [(c, reverse x)]
        encodeTree' (Branch l r _) x = encodeTree' l (O:x) ++ encodeTree' r (I:x)
    

-- | The 'tupleToMap' function is a wrapper for Data.Map.fromList. This is necessary for encoding the original text. 
tupleToMap  :: [(Char, [Binary])] -- ^ List of tuples containing character and binary code
            -> Map Char [Binary]  -- ^ Map (AKA dictionary) where key is unique character, and value binary code
tupleToMap = fromList

-- | The 'encodeString' function completes Huffman Encoding on text using a Map (see 'encodeTree' and subsequently 'tupleToMap')
encodeString    :: String               -- ^ Text
                -> Map Char [Binary]    -- ^ Map (AKA dictionary) where key is a unique character, and value a binary code
                -> [Binary]             -- ^ Huffman encoded Text
encodeString [] _ = []
encodeString (x:xs) m = m ! x ++ encodeString xs m

-- | The 'decodeList' function decodes a Huffman code using Huffman Codes in the form of a Map. 
-- | Tries to find keys in the inputted list. If it cannot, it errors out.
decodeList  :: [Binary]                 -- ^ Binary code
            -> Map [Binary] Char        -- ^ Map (AKA dictionary) where key is a binary code, and value is a unique character
            -> String                   -- ^ Huffman decoded Text
decodeList = decodeList' []
    where
        decodeList' [] [] _ = []
        decodeList' xs [] m 
            | xs `member` m = m ! xs : decodeList' [] [] m
            | otherwise     = error "decodeList; Bad Huffman key"
        decodeList' xs xss m 
            | xs `member` m = (m ! xs) : decodeList' [] xss m 
            | otherwise     = decodeList' (xs ++ [head xss]) (tail xss) m

-- | Returns a Haskell encoded String
-- Composition of 'encodeString', 'tupleToMap', 'encodeTree', 'createTree', 'createNodes' and 'countValues'
codedString :: String   -- ^ String for encoding
            -> [Binary] -- ^ Binary code representing string
codedString x = encodeString x $ tupleToMap $ encodeTree $ createTree $ createNodes $ countValues x 

-- Encoding:
-- countValues (String) -> createNodes -> createTree -> encodeTree -> encodeString