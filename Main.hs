{-# LANGUAGE NoMonomorphismRestriction #-} 
{-# LANGUAGE FlexibleInstances #-}

import Data.Char
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import System.IO
import Control.Monad.State
import Data.Generics.Uniplate.Direct
import Debug.Trace
import Control.Applicative
import Control.Arrow

{-
main = domain "testinput.html" 

domain str = do  
  handle <- openFile str ReadMode
  handleOut <- openFile "out.html" WriteMode
  hSetEncoding handle utf8_bom
  hSetEncoding handleOut utf8_bom
  contents <- hGetContents handle
  let (stuff,(more,more2)) = runState (f $ head $ tagTree $ parseTags contents) (  0 ,[] ) 
  let html = renderTags $ flattenTree $ (:[]) $ stuff
  forM more2 $ \(cont)  -> do
    let serial = 0
    handleContent <- openFile ("content_"++(show serial)++".html") WriteMode
    hSetEncoding handleContent utf8_bom
    hPutStr handleContent (renderTags cont)
    hClose handleContent
  hSetEncoding handle utf8_bom
  hSetEncoding stdout utf8_bom
  hPutStr handleOut html
  hClose handleOut

templatify contents = ( renderTags $ flattenTree $ pure r,  s)
  where (r,s) = templatify' contents

templatify' contents = (r, snd s)
  where (r,s) = runState (f $ head $ tagTree$ parseTags contents) (  0 ,[] )

-}
{-
templateify_ contents = ( renderTags $ flattenTree $ pure r,  s)
  where (r,s) = templateify_' contents
-}

 {-

templateify_'  contents = (map fst z,  map ( snd. snd) z)
  where z = templateify_'' contents 

templateify_'' contents = map templateify_''' (tagTree $ parseTags contents)

templateify_''' contents  = runState (f contents) ( 0 ,[] )

f t@(TagBranch name atts ts)  
    = mapM (modify (\(x,y)->(x+1,ts++y)) >> get >>= \(counter,areas) -> return (TagBranch name atts [(TagLeaf (TagText ("{{area"++show counter++"}")))])
 | otherwise =  mapM (descendM f) ts >>= \res -> return (TagBranch name atts res)
 where 
  (gs,bs) = partition p ts
  fts = map flattenTreeEasy ts
  p x = (not (any containsContainers x)) && (not (all allWhiteSpace x))
f x = return x
 -}


listToEither p xs = map (\x -> case p x of {True -> Left x ; _ -> Right x} ) xs

containsContainers = 
 any (or . mapM isTagOpenName ["div","table","td","tr","thead","tbody"])

allWhiteSpace flatTree = 
    (all (or . sequence [isTagOpenName "br",isTagText]) flatTree)
 && (all isSpace . innerText $ flatTree)

flattenTreeEasy x = flattenTree [x] 
 
instance Uniplate (TagTree a) where
  uniplate (TagBranch x1 x2 x3) = plate (TagBranch x1 x2) ||* x3
  uniplate x = plate x   
