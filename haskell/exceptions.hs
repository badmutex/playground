{-# LANGUAGE
  GeneralizedNewtypeDeriving
  , NoMonomorphismRestriction
  #-}

import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.CatchIO
import Control.Exception (IOException)
import qualified Control.Exception as E
import System.Exit
import qualified System.IO.Error as IO
import Text.Printf


safeLiftIO = liftFail . liftIO . wrapIO

justIO :: IOException -> Maybe String
justIO e@(_) = Just $ show e
 
wrapIO :: IO a -> IO (Either String a)
wrapIO = tryJust justIO
 
liftFail :: MonadIO m => m (Either String a) -> m a
liftFail m = m >>= lifter
    where lifter (Left msg) = fail msg
          lifter (Right v) = return v

newtype M a = M {
      runM :: ErrorT String IO a
    } deriving (MonadIO, Monad, MonadCatchIO)

run :: M a -> IO (Either String a)
run m = runErrorT . runM $ m


badIO :: IO String
badIO = readFile badfile
    where
      badfile = "/tmp/bad"

--badM :: IO (Either E.ErrorCall String)
badM = liftIO $ try badIO