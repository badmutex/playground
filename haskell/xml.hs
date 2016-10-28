{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.XML.HXT.Arrow

import Text.ParserCombinators.Parsec

import System.Environment
import Data.Char
import Debug.Trace as D



data GProtein = Gq | Gs | Gi | G12 deriving (Eq, Show)
type Specificity = Double

newtype Result = Result [(GProtein, Specificity)]


test doc out = runX $ readDocument [(a_parse_html, "1"), (a_trace, "0")] doc
                  >>> deep isText
                  >>> writeDocument [(a_indent, "1")] out

test2 as doc = runX (rundoc as doc) >>= return . extract

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

specificity :: Parser String
specificity = do
  d <- many digit
  char '.'
  d' <- many digit
  return $ d ++ "." ++ d'

gprotein :: String -> Parser String
gprotein s = do
  string $ s ++ " - \t"
  specificity

gq = gprotein "Gq/11"
gi = gprotein "Gi/o"
gs = gprotein "Gs"
g12 = gprotein "G12/13"





extract = concat




main = do
  [doc,out] <- getArgs
  test doc out
