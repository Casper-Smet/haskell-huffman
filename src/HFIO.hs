module HFIO
    ( keyFromString
    , reshapeCodes
    , formatCodes
    , listifyCodes
    , readCodes
    , readCodeString
    ) where

import Data.List.Split              (splitOn)

-- -- TODO: Add comments
-- -- TODO: Add check for in '0,1'
delimiter1 = "||@||"
delimiter2 = "//#//"

keyFromString :: String -> [Int]
keyFromString key = [read [k] :: Int | k <- key]

reshapeCodes :: [(Char, [Int])] -> [(Char, String)]
reshapeCodes xs = [(fst x, concat $ show <$> snd x) | x <- xs]

-- FIXME: ++ and [c] are slow, very bad, and not good. Figure out an alternative. Maybe use ':' and reverse?
formatCodes :: [(Char, String)] -> String
formatCodes []            = []
formatCodes [(c, i)]      = [c] ++ delimiter1 ++ i
formatCodes ((c, i):xs)   = [c] ++ delimiter1 ++ i ++ delimiter2 ++ formatCodes xs

listifyCodes :: String -> [[String]]
listifyCodes xs = [splitOn delimiter1 x | x <- splitOn delimiter2 xs]

readCodes :: [[String]] -> [([Int], Char)]
readCodes xss = [readCode xs | xs <- xss]

readCode [c, i] = (keyFromString i, head c)
readCode _ = error "readCodes; Bad Huffman codes"

readCodeString x = readCodes $ listifyCodes x 
