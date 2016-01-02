{-# LANGUAGE NoMonomorphismRestriction #-} 
{-# LANGUAGE FlexibleInstances #-}

import Data.Char
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import System.IO
import Control.Monad.RWS
import Data.Generics.Uniplate.Direct

main = do
  -- simpleHttp "https://en.wikipedia.org/wiki/Main_Page"
  handle <- openFile "testinput.html" ReadMode
  handleOut <- openFile "out.html" WriteMode
  hSetEncoding handle utf8_bom
  hSetEncoding handleOut utf8_bom
  contents <- hGetContents handle
  let html = renderTags $  flattenTree $ (:[])$ transform f $ head $  tagTree $ parseTags contents
  hSetEncoding stdout utf8_bom
  hPutStr handleOut html

f subtree 
 | isContent subtree = (TagLeaf (TagText "blah"))
 | otherwise = subtree 

isContent subtree =
 not (containsContainers flatTree)
 && not(isAllWhiteSpace flatTree)
 where 
  flatTree = flattenTreeEasy subtree

containsContainers = 
   (not . null) .
   filter (or . mapM isTagOpenName ["div","table","td","tr","thead","tbody"])


isAllWhiteSpace = 
  and . 
  map (or . sequence [isTagOpenName "br" , allSpace . innerText . (:[]) ])

allSpace = all isSpace

flattenTreeEasy x = flattenTree [x] 
 
instance Uniplate (TagTree a) where
  uniplate (TagBranch x1 x2 x3) = plate (TagBranch x1 x2) ||* x3                                                                                                
  uniplate x = plate x   




