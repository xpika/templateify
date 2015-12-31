{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Char
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import System.IO



main = do
  -- simpleHttp "https://en.wikipedia.org/wiki/Main_Page"
  handle <- openFile "Main_Page.html" ReadMode
  hSetEncoding handle utf8_bom
  contents <- hGetContents handle
  let html = renderTags $ parseTags contents
  hSetEncoding stdout utf8_bom
  putStr html
  
