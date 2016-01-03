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
  hSetEncoding handle utf8_bom
  hSetEncoding handleOut utf8_bom
  contents <- hGetContents handle
  let (stuff,(more,more2)) = runState (f $ head $ tagTree $ parseTags contents) (  0 ,[] ) 
  let html = renderTags $ flattenTree $ (:[]) $ stuff
  forM more2 $ \(serial,cont )  -> do
    handleContent <- openFile ("content_"++(show serial)++".html") WriteMode
    hSetEncoding handleContent utf8_bom
    hPutStr handleContent (renderTags cont)
    hClose handleContent
  hSetEncoding handle utf8_bom
  hSetEncoding stdout utf8_bom
  hPutStr handleOut html
  hClose handleOut

f t@(TagBranch name atts ts)  
 | (not (any containsContainers fts)) && (not (all allWhiteSpace fts))
    = modify (\(x,y)->(x+1,(x+1,concat fts):y)) >> get >>= \(counter,areas) -> return (TagBranch name atts [(TagLeaf (TagText ("{{area"++show counter++"}")))])
 | otherwise =  mapM (descendM f) ts >>= \res -> return (TagBranch name atts res)
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
