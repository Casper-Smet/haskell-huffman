module Lib
    ( countValues
    , createNodes
    , createTree
    , encodeTree
    , encodeString
    , decodeList
    , tupleToMap
    , codedString
    ) where

import Data.List (group, sort)
import Data.Map (Map, fromList, lookup, (!), member)
import Control.Monad (join)


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

-- | The 'mergeNodes' function merges two HuffmanTree's into one Branch. 
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

encodeTree  :: HuffmanTree      -- ^ Completed HuffmanTree
            -> [(Char, [Int])]  -- ^ List of tuples containing character and path (binary code)
encodeTree t = encodeTree' t []
    where 
        encodeTree' (Node c _) x = [(c, reverse x)]
        encodeTree' (Branch l r _) x = encodeTree' l (0:x) ++ encodeTree' r (1:x)
    

-- TODO: Add pattern matching for Data.Map
-- | The 'tupleToMap' function is a wrapper for Data.Map.fromList. This is necessary for encoding the original text. 
tupleToMap  :: [(Char, [Int])] -- ^ List of tuples containing character and binary code
            -> Map Char [Int]  -- ^ Map (AKA dictionary) where key is unique character, and value binary code
tupleToMap = fromList

-- | The 'encodeString' function completes Huffman Encoding on text using a Map (see 'encodeTree' and subsequently 'tupleToMap')
encodeString    :: String           -- ^ Text
                -> Map Char [Int]   -- ^ Map (AKA dictionary) where key is a unique character, and value a binary code
                -> [Int]            -- ^ Huffman encoded Text
encodeString [] _ = []
encodeString (x:xs) m = m ! x ++ encodeString xs m

-- | The 'decodeList' function decodes a Huffman code using Huffman Codes in the form of a Map. 
-- | Checks if xs is a member of m. 
-- | If it is a member, it grabs that values calls itself again with an empty xs and xss as it is.
-- | If it isn't a member of m, it calls itself where xs = xs and the first value of xss, and xss is the tail of xss
decodeList  :: [Int]                -- ^ xss. Remainder of the text after head is removed for xs.
            -> Map [Int] Char       -- ^ Map (AKA dictionary) where key is a binary code, and value is a unique character
            -> String               -- ^ Huffman decoded Text
decodeList = decodeList' []
    where
        decodeList' [] [] _ = []
        decodeList' xs [] m = m ! xs : decodeList' [] [] m
        decodeList' xs xss m 
            | xs `member` m = (m ! xs) : decodeList' [] xss m 
            | otherwise     = decodeList' (xs ++ [head xss]) (tail xss) m


-- TODO: eta-reduction
treeString x = createTree $ createNodes $ countValues x

codeString x = encodeTree (treeString x)

codeMap' x = tupleToMap $ codeString x

codedString x = encodeString x $ tupleToMap $ encodeTree $ createTree $ createNodes $ countValues x 
-- Encoding:
-- countValues (String) -> createNodes -> createTree -> encodeTree -> encodeString