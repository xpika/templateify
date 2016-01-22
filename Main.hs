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

eg = templateify egStr
egStr = "<div>hello<div>world</div></div>"

templateify str = (renderTags .flattenTree) *** (map (renderTags . flattenTree)) $ templateify' str
templateify' str = (concat  ***  snd) $ runState (f $ stringToTagTree str) (0,[])

stringToTagTree str = tagTree $ parseTags str

f ts = mapM (mapM innerDo ||| f') (shade p ts)
 where
  f' subtrees = do 
   modify (\(x,y)->(x+1,subtrees:y))
   (counter,areas) <- get 
   return [TagLeaf (TagText ("{{area"++show counter++"}"))]
  innerDo (TagBranch str atts children) = do 
    children' <- f children
    return (TagBranch str atts (concat children'))
  innerDo x = return x

p x = not ((containsContainers x') || (allWhiteSpace x'))
    where x' = flattenTreeEasy x

shade p = map (\xs -> if not . null $ filter p xs then Right xs else Left xs)  . groupBy (on (==) p) 

containsContainers = 
 any (or . mapM isTagOpenName ["div","table","td","tr","thead","tbody"])

allWhiteSpace flatTree = 
 (all isSpace . innerText $ flatTree)

flattenTreeEasy x = flattenTree [x] 
 
instance Uniplate (TagTree a) where
  uniplate (TagBranch x1 x2 x3) = plate (TagBranch x1 x2) ||* x3
  uniplate x = plate x   
