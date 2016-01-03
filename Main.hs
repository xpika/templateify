{-# LANGUAGE NoMonomorphismRestriction #-} 
{-# LANGUAGE FlexibleInstances #-}

import Data.Char
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import System.IO
import Control.Monad.RWS
import Data.Generics.Uniplate.Direct
import Debug.Trace

main = do
  handle <- openFile "testinput.html" ReadMode
  handleOut <- openFile "out.html" WriteMode
  hSetEncoding handle utf8_bom
  hSetEncoding handleOut utf8_bom
  contents <- hGetContents handle
  let html = renderTags $ flattenTree $ (:[]) $ transform f $ head $ tagTree $ parseTags contents
  hSetEncoding stdout utf8_bom
  hPutStr handleOut html
  hClose handleOut

f t@(TagBranch name atts ts)  
 | (not (any containsContainers fts)) && (not (all allWhiteSpace fts))
    = (TagBranch name atts [(TagLeaf (TagText "blah"))])
 | otherwise = t
 where 
  fts = map flattenTreeEasy ts
f x = x

containsContainers = 
 any (or . mapM isTagOpenName ["div","table","td","tr","thead","tbody"])

allWhiteSpace flatTree = 
    (all (or . sequence [isTagOpenName "br",isTagText]) flatTree)
 && (all isSpace . innerText $ flatTree)

flattenTreeEasy x = flattenTree [x] 
 
instance Uniplate (TagTree a) where
  uniplate (TagBranch x1 x2 x3) = plate (TagBranch x1 x2) ||* x3
  uniplate x = plate x   
