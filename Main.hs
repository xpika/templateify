{-# LANGUAGE NoMonomorphismRestriction #-} 
{-# LANGUAGE FlexibleInstances #-}

import Data.Char
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import System.IO
import Control.Monad.State
import Data.Generics.Uniplate.Direct
import Debug.Trace

main = do
  handle <- openFile "testinput.html" ReadMode
  handleOut <- openFile "out.html" WriteMode
  handlecontent <- openFile "content_.html" WriteMode
  hSetEncoding handle utf8_bom
  hSetEncoding handleOut utf8_bom
  contents <- hGetContents handle
  let (stuff,more) = runState (f $ head $ tagTree $ parseTags contents) (  0  ) 
  let html = renderTags $ flattenTree $ (:[]) $ stuff
  hSetEncoding stdout utf8_bom
  hPutStr handleOut html
  hClose handleOut
  hClose handlecontent

f t@(TagBranch name atts ts)  
 | (not (any containsContainers fts)) && (not (all allWhiteSpace fts))
    = modify (+1) >> get >>= \v -> return (TagBranch name atts [(TagLeaf (TagText ("blah"++show v)))])
 | otherwise =  mapM (descendM f) ts >>= \res -> return (TagBranch name atts (map id res))
 where 
  fts = map flattenTreeEasy ts
f x = return x

containsContainers = 
 any (or . mapM isTagOpenName ["div","table","td","tr","thead","tbody"])

allWhiteSpace flatTree = 
    (all (or . sequence [isTagOpenName "br",isTagText]) flatTree)
 && (all isSpace . innerText $ flatTree)

flattenTreeEasy x = flattenTree [x] 
 
instance Uniplate (TagTree a) where
  uniplate (TagBranch x1 x2 x3) = plate (TagBranch x1 x2) ||* x3
  uniplate x = plate x   
