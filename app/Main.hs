module Main where

import Lib
import System.IO
import Control.Monad.Trans       (lift)
import Control.Monad.Trans.State (StateT, runStateT, evalStateT, modify, get, put)
import Data.Map (Map, empty, fromList, lookup, (!), member, insert)

type DecodeMonad = StateT (Map [Int] Char, DecodeState) IO
type DecodeFileMonad = StateT (Map [Int] Char, String) IO

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
    let codes = encodeTree tree

    -- Step 5: turn tuples into Data.Map
    let codeMap = tupleToMap codes 

    -- Step 6: encode input
    let codedInput = encodeString input codeMap

    print codedInput
 

encodeFile :: IO ()
encodeFile = do
    putStrLn "Location of text file"
    location <- getLine
    -- Read contents of file
    contents <- readFile location
    -- Turn contents into Map (dictionary) and Huffman Code
    putStrLn "Encoding text"
    let codeTuple = encodeTree $ createTree $ createNodes $ countValues contents
    let code = encodeString contents $ tupleToMap codeTuple  


    -- Write Map to file
    putStrLn "Writing Map to File"
    writeFile ("map" ++ location) $ show codeTuple

    -- Write String to file
    putStrLn "Writing code to File"
    writeFile ("code" ++ location) $ concat $ show <$> code



-- TODO: Clean up
-- TODO: add reads "10c" :: [(Integer, String)]
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
                        let decodedWord = decodeList list map
                        lift $ putStrLn decodedWord

decodeFile :: IO ()
decodeFile = do
    putStrLn "Relative path to codemap:"
    codeMapLocation <- getLine
    putStrLn "Relative path to code"
    codeLocation <- getLine
    putStrLn $ codeMapLocation ++ codeLocation
    

main :: IO ()
main = do
    putStrLn "Encode (E), Decode (D), Encode to file (EF), Decode from file (DF), Quit (ctrl-C)"
    input <- getLine
    case input of
        "E"     -> encode
        "D"     -> evalStateT decode (empty, Entering)
        "EF"    -> encodeFile
        "DF"    -> decodeFile
        _       -> putStrLn "Bad input"
    main
