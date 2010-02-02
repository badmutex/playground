{-# LANGUAGE 
  PackageImports
  , GeneralizedNewtypeDeriving
  #-}


import "mtl" Control.Monad.Trans
import "MonadCatchIO-mtl" Control.Monad.CatchIO
import Control.Monad.Writer (WriterT, runWriterT, MonadWriter, tell)
import Control.Monad.Loops
import Control.Exception (ErrorCall)

import Prelude hiding (catch)

type Mine = WriterT [String] IO


runMine :: Mine a -> IO (a, [String])
runMine m = runWriterT $ m

bad :: Mine Int
bad = do
  tell ["Hello"]
  liftIO $ readFile "/tmp/asdfasd"
  return 42

catcher :: Mine Int
catcher = catch bad catcher
    where catcher :: ErrorCall -> Mine Int
          catcher e = return 24

par :: Mine ()
par = liftIO $ forkMapM__ print [1..99]

test = runMine catcher