{-# LANGUAGE NoMonomorphismRestriction #-} {-# LANGUAGE FlexibleInstances #-} 
import Data.Char
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import System.IO
import Control.Monad.State
import Data.Generics.Uniplate.Direct
import Debug.Trace
import Control.Applicative
import Control.Arrow
import Data.Function
import Data.List

main = domain "testinput.html" 

domain str = do  
 handle <- openFile str ReadMode
 handleOut <- openFile "out.html" WriteMode
 hSetEncoding handle utf8_bom
 hSetEncoding handleOut utf8_bom
 sourceHtml <- hGetContents handle
 let (template, contents) = templateify sourceHtml
 forM (zip contents [1..]) $ \(content,serial)  -> do
  handleContent <- openFile ("content_"++(show serial)++".html") WriteMode
  hSetEncoding handleContent utf8_bom
  hPutStr handleContent content
  hClose handleContent
 hSetEncoding handle utf8_bom
 hSetEncoding stdout utf8_bom
 hPutStr handleOut template
 hClose handleOut

eg = templateify egStr
egStr = "<div>hello<div>world</div></div>"

templateify str = renderTags . flattenTree *** map (renderTags . flattenTree) $ templateify' str
templateify' str = concat *** snd $ runState (f $ stringToTagTree str) (0,[])

stringToTagTree str = tagTree $ parseTags str

f ts = mapM ( f' |||  mapM innerDo) (shade containsContainers ts)
  where
  f' subtrees | all allWhiteSpace subtrees = return subtrees
              | otherwise = f'' subtrees
  f'' subtrees = do 
   modify (\(x,y)->(x+1,subtrees:y))
   (counter,areas) <- get 
   return [TagLeaf (TagText ("{{area"++show counter++"}"))]
  innerDo (TagBranch str atts children) = do 
    children' <- f children
    return (TagBranch str atts (concat children'))
  innerDo x = return x

shade p = map (\xs -> if not . null $ filter p xs then Right xs else Left xs) . groupBy (on (==) p)  
color p a = if p a then Right a else Left a 

containsContainers x = (containsContainers' x')
  where
  x' = flattenTreeEasy x

containsContainers' = 
 any (or . mapM isTagOpenName ["div","table","td","tr","thead","tbody"])

allWhiteSpace x = allWhiteSpace' x'
  where 
  x' = flattenTreeEasy x

allWhiteSpace' flatTree = 
  (all isSpace . innerText $ flatTree)

flattenTreeEasy x = flattenTree [x] 
 
instance Uniplate (TagTree a) where
  uniplate (TagBranch x1 x2 x3) = plate (TagBranch x1 x2) ||* x3
  uniplate x = plate x
