-- import Test.QuickCheck
import Test.HUnit
import Data.Map (Map, fromList)

import Lib 
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
    )

import HFIO 
    ( keyFromString
    , reshapeCodes
    , formatCodes
    , listifyCodes
    , readCodes
    , readCodeString
    )
    -- db   db db    db d8b   db d888888b d888888b 
    -- 88   88 88    88 888o  88   `88'   `~~88~~' 
    -- 88ooo88 88    88 88V8o 88    88       88    
    -- 88~~~88 88    88 88 V8o88    88       88    
    -- 88   88 88b  d88 88  V888   .88.      88    
    -- YP   YP ~Y8888P' VP   V8P Y888888P    YP    

nodeList :: [HuffmanTree]
nodeList = [Node 'h' 1, Node 'u' 1, Node 'f' 2]

tree :: HuffmanTree
tree = Branch (Branch (Node 'h' 1) (Node 'u'  1) 2) (Node 'f' 2) 4

enMap :: Map Char [Binary]
enMap = fromList [('f',[I]),('h',[O,O]),('u',[O,I])]

deMap :: Map [Binary] Char
deMap = fromList [([I], 'f'),([O,O], 'h'),([O,I],'u')]

-- Lib.hs
-- | countValues "huffman" == [('H',1),('a',1),('f',2),('m',1),('n',1),('u',1)]
countValuesTest :: Test
countValuesTest = TestCase (assertEqual "countValues" [('H',1),('a',1),('f',2),('m',1),('n',1),('u',1)] (countValues "Huffman"))

-- | createNodes [('h', 1)] == [Node 'h' 1]
createNodesTest :: Test
createNodesTest = TestCase (assertEqual "createNodes" [Node 'h' 1] (createNodes [('h', 1)]))

-- | mergeNodes (Node 'h' 1)  (Node 'u' 1) == Branch (Node 'h' 1) (Node 'u' 1) 2
mergeNodesTest :: Test
mergeNodesTest = TestCase (assertEqual "mergeNodes" (Branch (Node 'h' 1) (Node 'u' 1) 2) (mergeNodes (Node 'h' 1)  (Node 'u' 1)))

-- | createTree [Node 'h' 1, Node 'u' 1, Node 'f' 2] == Branch (Branch (Node 'h' 1) (Node 'u'  1) 2) (Node 'f' 2) 4
createTreeTest :: Test
createTreeTest = TestCase (assertEqual "createTree" tree (createTree nodeList))

-- | encodeTree Branch (Branch (Node 'h' 1) (Node 'u'  1) 2) (Node 'f' 2) 4 == [('h',[O,O]),('u',[O,I]),('f',[I])]
encodeTreeTest :: Test
encodeTreeTest = TestCase (assertEqual "encodeTree" [('h',[O,O]),('u',[O,I]),('f',[I])] (encodeTree tree))

-- | encodeString "huff" (fromList [('f',[I]),('h',[O,O]),('u',[O,I])]) == [O,O,O,I,I,I]
encodeStringTest :: Test
encodeStringTest = TestCase (assertEqual "encodeString" [O,O,O,I,I,I] (encodeString "huff" enMap))

-- | decodeList [I,O,I,O,O] (fromList [([I], 'f'),([O,O], 'h'),([O,I],'u')]) == "fuh"
decodeListTest :: Test
decodeListTest = TestCase (assertEqual "decodeList" "fuh" (decodeList [I,O,I,O,O] deMap))

-- | codedString "huffman coding" == [I,O,O,I,I,I,O,I,O,I,I,O,I,I,I,I,I,I,O,I,O,I,O,O,O,I,O,O,I,O,I,O,I,I,O,O,I,O,I,I,I,I,I,O,O,O,I,O,O,O]
codedStringTest :: Test
codedStringTest = TestCase (assertEqual "codedString" [I,O,O,I,I,I,O,I,O,I,I,O,I,I,I,I,I,I,O,I,O,I,O,O,O,I,O,O,I,O,I,O,I,I,O,O,I,O,I,I,I,I,I,O,O,O,I,O,O,O] (codedString "huffman coding"))

-- HFIO.hs
-- | keyFromString "IOIO" == [I,O,I,O]
keyFromStringTest :: Test
keyFromStringTest = TestCase (assertEqual "keyFromString" [I,O,I,O] (keyFromString "IOIO"))

-- | reshapeCodes [('c', [I,O,I])] == [('c', "IOI")]
reshapeCodesTest :: Test
reshapeCodesTest = TestCase (assertEqual "reshapeCodes" [('c', "IOI")] ( reshapeCodes [('c', [I,O,I])]))

-- | formatCodes [('c', "IOI"), ('d', "IIO")] == "c||IOI#|/d||IIO"
formatCodesTest :: Test
formatCodesTest = TestCase (assertEqual "formatCodes" "c||IOI#|d||IIO" (formatCodes [('c', "IOI"), ('d', "IIO")]))

-- | listifyCodes "c||IOI#|/d||IIO" == [["c", "IOI"], ["d", "IIO"]]
listifyCodesTest :: Test
listifyCodesTest = TestCase (assertEqual "listifyCodes" [["c", "IOI"], ["d", "IIO"]] (listifyCodes "c||IOI#|d||IIO"))

-- | readCodes  [["c", "IOI"], ["d", "IIO"]] == [([I,O,I], 'c'), ([I,I,O], 'd')]
readCodesTest :: Test
readCodesTest = TestCase (assertEqual "readCodes" [([I,O,I], 'c'), ([I,I,O], 'd')] (readCodes  [["c", "IOI"], ["d", "IIO"]]))

-- | readCodeString "c||IOI#|d||IIO" ==  [([I,O,I], 'c'), ([I,I,O], 'd')]
readCodeStringTest :: Test
readCodeStringTest = TestCase (assertEqual "readCodeString" [([I,O,I], 'c'), ([I,I,O], 'd')] (readCodeString "c||IOI#|d||IIO"))


tests = TestList [
    -- Lib tests
    TestLabel "Lib.hs(countValues)" countValuesTest,
    TestLabel "Lib.hs(createNodes)" createNodesTest, 
    TestLabel "Lib.hs(mergeNodes)" mergeNodesTest,
    TestLabel "Lib.hs(createTree)" createTreeTest,
    TestLabel "Lib.hs(encodeTree)" encodeTreeTest,
    TestLabel "Lib.hs(encodeString)" encodeStringTest,
    TestLabel "Lib.hs(decodeList)" decodeListTest,
    TestLabel "Lib.hs(codedString)" codedStringTest,
    -- HFIO test
    TestLabel "HFIO.hs(keyFromString)" keyFromStringTest,
    TestLabel "HFIO.hs(reshapeCodes)" readCodeStringTest,
    TestLabel "HFIO.hs(formatCodes)" formatCodesTest,
    TestLabel "HFIO.hs(listifyCodes)" listifyCodesTest,
    TestLabel "HFIO.hs(readCodes)" readCodesTest,
    TestLabel "HFIO.hs(readCodeString)" readCodeStringTest
    ]

main :: IO Counts
main = runTestTT tests 
    

