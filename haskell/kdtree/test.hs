{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.Monad
import System.Random
import Data.List (foldl')
import qualified Data.KdTree.Dynamic as KDT


data P2 = P2 {-# UNPACK #-} !Int
             {-# UNPACK #-} !Int
        deriving Show

test = do
  setStdGen $ mkStdGen 42
  points <- forM [1..10] $ \_ -> do
              P2 <$> randomRIO (0, 10) <*> randomRIO (0, 10)
  return $ foldl' KDT.insert (KDT.empty (\(P2 x y) -> [x,y])) points
