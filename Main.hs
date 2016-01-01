{-# LANGUAGE NoMonomorphismRestriction #-} 


import Data.Char
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import System.IO

main = do
  -- simpleHttp "https://en.wikipedia.org/wiki/Main_Page"
  handle <- openFile "Main_Page.html" ReadMode
  handleOut <- openFile "out.html" WriteMode
  hSetEncoding handle utf8_bom
  hSetEncoding handleOut utf8_bom
  contents <- hGetContents handle
  let html = renderTags $ flattenTree $ transformTree f $  tagTree $ parseTags contents
  hSetEncoding stdout utf8_bom
  hPutStr handleOut html

flattenTreeEasy x  = flattenTree [x]
 
f subtree 
 | isContent subtree = [] 
 | otherwise = [subtree] 

isContent =  
 (not . null) .
 filter (or . mapM isTagOpenName ["div","table","td","tr","thead","tbody"]) . 
 flattenTreeEasy
 
