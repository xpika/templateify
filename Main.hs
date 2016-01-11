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
import Data.Function
import Data.List



shade p = map (\xs -> if not . null $ filter p xs then Left xs else Right xs)  . groupBy (on (==) p) 

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
-}


{-
f ts = mapM (f' ||| mapM (descendM f) ) (shade p ts) >>= return . concat
 where
  f' subtrees = do 
    mapM (\subtree -> modify (\(x,y)->(x+1,subtree:y))) subtrees 
    (counter,areas) <- get 
    return [TagLeaf (TagText ("{{area"++show counter++"}"))]
  p x = (not (containsContainers x')) && (not (allWhiteSpace x'))
    where x' = flattenTreeEasy x
f x = return x
-}


containsContainers = 
 any (or . mapM isTagOpenName ["div","table","td","tr","thead","tbody"])

allWhiteSpace flatTree = 
    (all (or . sequence [isTagOpenName "br",isTagText]) flatTree)
 && (all isSpace . innerText $ flatTree)

flattenTreeEasy x = flattenTree [x] 
 
instance Uniplate (TagTree a) where
  uniplate (TagBranch x1 x2 x3) = plate (TagBranch x1 x2) ||* x3
  uniplate x = plate x   
