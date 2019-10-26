module Main where

import Lib
import Control.Monad.Trans       (lift)
import Control.Monad.Trans.State (StateT, runStateT, modify, get, put)
import Data.Map (Map, empty, fromList, lookup, (!), member, insert)

type DecodeMonad = StateT (Map [Int] Char, DecodeState) IO
data DecodeState = Entering | Decoding deriving (Eq)

getDecState :: (Map [Int] Char, DecodeState) -> DecodeState
getDecState (_, s) = s

setDecode :: (Map [Int] Char, DecodeState) -> (Map [Int] Char, DecodeState)
setDecode (m, _) = (m, Decoding) 

addKey ::  ([Int], Char) -> (Map [Int] Char, DecodeState) -> (Map [Int] Char, DecodeState)
addKey (xs, c) (m, s) = (insert xs c m, s) 

getMap :: (Map [Int] Char, DecodeState) -> Map [Int] Char
getMap (m, _) = m


encode :: IO ()
encode = do
    putStrLn "Enter String for Huffman coding"
    input <-  getLine
    -- Step 1: count each unique value
    let valueCounts = countValues input

    -- Step 2: create a node for each unique value
    let nodes = createNodes valueCounts

    -- Step 3: create Huffman Tree
    let tree = createTree nodes

    -- Step 4: encode Huffman Tree
    let codes = encodeTree tree []

    -- Step 5: turn tuples into Data.Map
    let codeMap = tupleToMap codes 

    -- Step 6: encode input
    let codedInput = encodeString input codeMap

    print codedInput
 

decode :: DecodeMonad ()
decode = do   
                state <- get  
                if getDecState state == Entering 
                then do
                        lift $ putStrLn "Enter character"
                        value <- lift getLine
                        lift $ putStrLn "Enter code"
                        key <- lift getLine
                        if all (`elem` "01") key && length value == 1
                            then do
                                let intKey = [read [k] :: Int | k <- key]
                                let charValue = head value
                                modify (addKey (intKey, charValue))
                                decode
                        else do
                                modify setDecode
                                decode
                else
                    do
                        lift $ print $ getMap state
                        lift $ putStrLn "Enter encoded word"
                        word <- lift getLine
                        let list = [read [x] :: Int | x <- word]
                        let map = getMap state 
                        let decodedWord = decodeList [] list map
                        lift $ putStrLn decodedWord


-- FIXME: Add the ability to choose between encode and decode in main
-- main :: IO ()
-- main = do
--     putStrLn "Encode (E) or Decode (D)"
--     input <- getLine
--     if input == "E" then
--         encode       
--     else if input == "D" then
--         runStateT decode empty
--     else
--         main

main = runStateT decode (empty, Entering)