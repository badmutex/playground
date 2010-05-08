{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

import Prelude hiding (Monad, return, (>>=), (>>), fail, mod)

import Test.QuickCheck
import Debug.Trace



class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    ma >> mb = ma >>= \_ -> mb


newtype State s a = State { runState :: s -> (s, a) }


instance Functor (State s) where
    fmap f m = State $ \s -> let (s', a) = runState m s
                             in (s', f a)


instance Monad (State s) where
    return v = State (\s -> (s, v))
    m >>= f = State $ \s -> let (s', a) = runState m s
                            in runState (f a) s'


evalState :: State s a -> s -> a
evalState m s = snd (runState m s)

execState :: State s a -> s -> s
execState m s = fst (runState m s)



class Monad m => MonadState m s | m -> s where
    get :: m s
    put :: s -> m ()
    mod :: (s -> s) -> m ()
    mod f = get >>= (put . f)

instance MonadState (State s) s where
    get   = State $ \s -> (s, s)
    put s = State $ \_ -> (s, ())



type Test = State Int

inc :: Test ()
inc = mod (+1)

inc' :: Test ()
inc' = fmap (+1) get >>= return >>= put



prop_test :: Int -> Bool
prop_test s = let t = (==) (s+1) . flip execState s
                  xs = map t [inc, inc']
              in and xs





newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad Maybe where
    return = Just
    (Just a) >>= f = f a
    Nothing  >>= _ = Nothing

instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . Just
    (>>=)  = maybeTbind

maybeTbind m f = MaybeT $ runMaybeT m >>= \b ->
                 case b of Nothing -> return Nothing
                           Just v  -> runMaybeT $ f v


