{-# LANGUAGE
  TemplateHaskell,
  TypeOperators
  #-}
  

import Data.Default
import Data.Record.Label
import Language.Haskell.TH

data GlobalOptions = Global {
      _honchDir     :: FilePath
    , _makeflowPath :: FilePath
    }

$(mkLabels [''GlobalOptions])
honchDir     :: GlobalOptions :-> FilePath
makeflowPath :: GlobalOptions :-> FilePath

instance Default GlobalOptions where
    def = Global {
            _honchDir     = ".honcho"
          , _makeflowPath = "makeflow"
          }
