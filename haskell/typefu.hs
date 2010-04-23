{-# LANGUAGE
  GeneralizedNewtypeDeriving
  , ExistentialQuantification
  #-}


import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer



-- data Show c => MyConfig c = Mk { config' :: c }

-- newtype MyMonad c a = MkM {
--       runM :: ErrorT String (ReaderT (MyConfig c) IO) a
--     } deriving (Functor, Monad, MonadError String, MonadReader (MyConfig c), MonadIO)

-- runMyMonad config monad = flip runReaderT config . runErrorT $ runM monad

-- data Foo = Foo {foo_a :: String, foo_b :: [Int]} deriving Show
-- data Bar = Bar {bar_a :: [Int], bar_b :: String} deriving Show

-- data SomeMyConfig = forall c. SomeMyConfig (MyConfig c)

-- c1 :: MyConfig Foo
-- c2 :: MyConfig Bar
-- c1 = Mk $ Foo "hello" [1,2,3]
-- c2 = Mk $ Bar [4,5,6] "world"


-- cm1 :: MyMonad Foo Int
-- cm1 = do c <- ask
--          return 42

-- cm2 :: MyMonad Bar Int
-- cm2 = ask >> return 24

-- cm :: MyMonad c Int
-- cm = ask >> return 42

-- switch :: MonadPlus m => Bool -> m a -> m a
-- switch b m = if b then m else mzero




-- -- exec p = do switch p $ return $ runMyMonad c1 m
-- --             switch (not p) $ runMyMonad c2 m









data Globals = Globals { global :: String } deriving Show


newtype GlobalMonad a = GM {
      unGM :: ReaderT Globals IO a
    } deriving (Functor, Monad, MonadReader Globals, MonadIO)

runGM :: Globals -> GlobalMonad a -> IO a
runGM c = flip runReaderT c . unGM


data VMDConfig = VMDCfg {psf, pdb, dcd :: FilePath} deriving Show


newtype VMDMonad a = VMD {
      unVMD :: ErrorT String (ReaderT VMDConfig GlobalMonad) a
    } deriving (Functor, Monad, MonadError String, MonadReader VMDConfig, MonadIO)

runVMD :: Globals -> VMDConfig -> VMDMonad a -> IO (Either String a)
runVMD g c = runGM g . flip runReaderT c .  runErrorT . unVMD


data FaHConfig = FaHCfg {run, clone, gen :: Int} deriving Show

newtype FaHMonad a = FaH {
      unFaH :: ErrorT String (ReaderT FaHConfig GlobalMonad) a
    } deriving (Functor, Monad, MonadError String, MonadReader FaHConfig, MonadIO)

runFaH :: Globals -> FaHConfig -> FaHMonad a -> IO (Either String a)
runFaH g c = runGM g . flip runReaderT c . runErrorT . unFaH



data HaseemConfig = HCfg { hcfg :: String } deriving Show


newtype Haseem m a = Haseem {
      unHaseem :: ErrorT String (ReaderT HaseemConfig m) a
    } deriving (Functor, Monad, MonadError String, MonadReader HaseemConfig, MonadIO)


cg :: Globals
c1 :: VMDConfig
c2 :: FaHConfig
cg = undefined
c1 = undefined
c2 = undefined

m1 :: VMDMonad Int
m1 = return 42

m2 :: FaHMonad Int
m2 = return 24

exe p = if p
        then runVMD cg c1 m1
        else runFaH cg c2 m2