{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.XML.HXT.Arrow

import System.Environment
import Data.Char
import Debug.Trace as D



test doc out = runX $ readDocument [(a_parse_html, "1"), (a_trace, "0")] doc
                  >>> deep isText
                  >>> writeDocument [(a_indent, "1")] out

test2 as doc = runX (rundoc as doc)

don't_keep = ["\t","\n","\n\t"]

rundoc as src = application as src 


application al src
    = readDocument al src
      >>>
      processChildren getNodes
      >>>
      changeText reverse
      >>>
      processNodes

getNodes = deep (hasAttr "color" `when` isText)
processNodes = deep getText




main = do
  [doc,out] <- getArgs
  test doc out
