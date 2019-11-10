-- import Test.QuickCheck
import Test.HUnit
import Data.Map (Map, fromList)

import Lib ( countValues
    , createNodes
    , createTree
    , encodeTree
    , encodeString
    , decodeList
    , tupleToMap
    , codedString
    , mergeNodes
    , Binary(I, O)
    , HuffmanTree(Node, Branch))

nodeList :: [HuffmanTree]
nodeList = [Node 'h' 1, Node 'u' 1, Node 'f' 2]

tree :: HuffmanTree
tree = Branch (Branch (Node 'h' 1) (Node 'u'  1) 2) (Node 'f' 2) 4

enMap :: Map Char [Binary]
enMap = fromList [('f',[I]),('h',[O,O]),('u',[O,I])]

deMap :: Map [Binary] Char
deMap = fromList [([I], 'f'),([O,O], 'h'),([O,I],'u')]

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

tests = TestList [
    -- Lib tests
    TestLabel "countValues" countValuesTest,
    TestLabel "createNodes" createNodesTest, 
    TestLabel "mergeNodes" mergeNodesTest,
    TestLabel "createTree" createTreeTest,
    TestLabel "encodeTree" encodeTreeTest,
    TestLabel "encodeString" encodeStringTest,
    TestLabel "decodeList" decodeListTest,
    TestLabel "codedString" codedStringTest
    ]

main :: IO Counts
main = runTestTT tests 
    

