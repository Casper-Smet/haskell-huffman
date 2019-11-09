{-|
Module      : HFIO
Description : Module containing helper functions for encoding and decoding to file
Copyright   : (c) Casper Smet, 2019
License     : BSD3
Maintainer  : casper.smet@student.hu.nl

This module contains helper functions for encoding and decoding to file. Actual writing to file only happens in the application side of this library
This module was created for the Utrecht University of Applied Sciences AI course 'Declarative Programming'

There are a couple of references to \'delimiter1\' and \'delimiter2\'. These are defined as follows:

* delimiter1 = "||\@||"
* delimiter2 = "\/\/#\/\/"

-}
module HFIO
    ( keyFromString
    , reshapeCodes
    , formatCodes
    , listifyCodes
    , readCodes
    , readCodeString
    ) where

import Data.List.Split              (splitOn)

-- ####  #######  
--  ##  ##     ## 
--  ##  ##     ## 
--  ##  ##     ## 
--  ##  ##     ## 
--  ##  ##     ## 
-- ####  ####### 


-- | First delimiter used for encoding map \"||\@||\"
delimiter1 = "||@||"
-- | Second delimiter used for encoding map \"\//#\//\"
delimiter2 = "//#//"

-- | Takes a string containing \'1\' or \'0\'s, returns list of Int. 
keyFromString   :: String       -- ^ String, example: "0101"
                -> [Int]        -- ^ List of Int, example: [0,1,0,1]
keyFromString key 
    | all (`elem` "01") key = [read [k] :: Int | k <- key]
    | otherwise             = error "keyFromString; Bad Huffman key"

-- | Reshapes Huffman Map
reshapeCodes    :: [(Char, [Int])]      -- ^ Huffman Map in tuple form, example: [(\'c\', [1,0,1])]
                -> [(Char, String)]     -- ^ Huffman Map in Tuple form, example: [(\'c\', \"101\")]
reshapeCodes xs = [(fst x, concat $ show <$> snd x) | x <- xs]


-- | Takes a list of Huffman Key-Value pairs, turns into String for printing to file. Uses delimiter1 and delimiter2.
formatCodes    :: [(Char, String)]         -- ^ Huffman Map in tuple form, example: [(\'c\', "101"), (\'d\', "110")]
                -> String                   -- ^ Stringified Huffman map for printing to file, example: \"c||\@||101\/\/\#\/\/d||\@||110\"
formatCodes [] = []
formatCodes xs = concat $ formatCodes' xs
        where
            formatCodes' :: [(Char, String)] -> [String]
            formatCodes' [(c, i)] = [ c : delimiter1 ++ i]
            formatCodes' ((c, i):xs) = ( c : delimiter1 ++ i ++ "//#//") : formatCodes' xs


-- | Inverse of 'formatCodes' using lists instead of tuples
listifyCodes    :: String       -- ^ Stringified Huffman map, example: \"c||\@||101\/\/\#\/\/d||\@||110\"
                -> [[String]]   -- ^ Two dimensional list containing String-code pairs, example: [[\"c\", \"101\"], [\"d\", \"110\"]] 
listifyCodes xs = [splitOn delimiter1 x | x <- splitOn delimiter2 xs]

-- | Takes two-dimensional list, returns tuple pair (Huffman Map)
readCodes   :: [[String]]       -- ^ Two dimensional list containg String-code pairs, example [[\"c\", \"101\"], [\"d\", \"110\"]] 
            -> [([Int], Char)]  -- ^ Huffman Map in tuple form, example: [([1,0,1], \'c\'), ([1,1,1], \'d\')]
readCodes xss = [readCode xs | xs <- xss]

readCode :: [String] -> ([Int], Char)
readCode [c, i] = (keyFromString i, head c)
readCode _ = error "readCodes; Bad Huffman codes"

-- | Composition of 'readCodes' and 'listifyCodes'
readCodeString  :: String           -- ^ Stringified Huffman map, example: \"c||\@||101\/\/\#\/\/d||\@||110\"
                -> [([Int], Char)]  -- ^ Huffman Map in tuple form, example: [([1,0,1], \'c\'), ([1,1,1], \'d\')]
readCodeString = readCodes . listifyCodes
