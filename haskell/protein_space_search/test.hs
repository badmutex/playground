{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

import Bio.PDB
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.ST
import qualified Data.ListLike as LL
import Data.List
import Data.STRef
import Data.String
import Data.Vector.Class
import Data.Vector.V3 as V3
import Foreign.Storable
import qualified Numeric.LinearAlgebra.HMatrix as HMatrix
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM

import Control.Monad.Primitive

pdb_path = "ala.pdb"

_positions :: Iterable a Atom => a -> HMatrix.Matrix Double
_positions it = HMatrix.reshape 3 $ runST $ do
                 xyz <- VSM.replicate (3 * numAtoms it) 0
                 itfoldM (\() atom -> do
                            let atmID = atSerial atom - 1
                                kf    = zip [0..2] [v3x, v3y, v3z]
                                crd   = coord atom
                            forM_ kf $ \(crdID, crdFN) -> do
                              let i = atmID * 3 + crdID
                              VSM.write xyz i $ crdFN crd
                         ) () it
                 VS.freeze xyz

data MatrixOrder = RowMajor | ColumnMajor deriving (Eq, Show)

data M a = Matrix {
      nrows :: {-# UNPACK #-} !Int
    , ncols :: {-# UNPACK #-} !Int
    , mdata :: {-# UNPACK #-} !a
    , morder:: !MatrixOrder
    }

matsize :: M a -> (Int, Int)
matsize m = (nrows m, ncols m)

showMatsize :: M a -> String
showMatsize m = s
    where (x,y) = matsize m
          s = "(" ++ show x ++ "><" ++ show y ++ ")"

zipMatrix :: (LL.ListLike vx x, LL.ListLike vy y, LL.ListLike vxy (x,y)) => M vx -> M vy -> M vxy
zipMatrix xs ys = if matsize xs /= matsize ys
                  then error $ "Size mismatch: " ++ showMatsize xs ++ " =/= " ++ showMatsize ys
                  else Matrix {
                             nrows = nrows xs
                           , ncols = ncols xs
                           , mdata = LL.zip (mdata xs) (mdata ys)
                           , morder = morder xs
                           }

mapMatrix :: (LL.ListLike vx x, LL.ListLike vy y) => (x -> y) -> M vx -> M vy
mapMatrix f m = m { mdata = data' }
    where data' = LL.map f (mdata m)

instance (LL.ListLike full item, Show item) => Show (M full) where
    show m = let unwords' = concat .  intersperse (", " :: String)
             in runST $ do
               list <- newSTRef (mdata m)
               string <- newSTRef []
               prefix <- newSTRef "["
               modifySTRef string ( [showMatsize m] ++ )
               forM_ [0..nrows m - 1] $ \r -> do
                          rest <- readSTRef list
                          let (row, rest') = LL.splitAt (ncols m) rest
                          modifySTRef list (const rest')
                          let str = unwords' . LL.fromList $ LL.map show row
                          pref <- readSTRef prefix
                          modifySTRef string (++ [" " ++ pref ++ str])
                          modifySTRef prefix (const ",")
               (++ "]") . unwords . intersperse "\n" <$> readSTRef string



matrix :: LL.ListLike full item => Int -> full  -> M full
matrix ncols v = if LL.length v `mod` ncols /= 0
                 then error $ "matrix: Length " ++ show (LL.length v) ++ " must be multiple of 'ncols' (" ++ show ncols ++ ")"
                 else Matrix {
                            nrows = LL.length v `div` ncols
                          , ncols = ncols
                          , mdata = v
                          , morder = RowMajor
                          }

selectAtom :: (Iterable a Atom, LL.ListLike v b) => (Atom -> Bool) -> (Atom -> [b]) -> Int -> a -> M v
selectAtom cond getter ncols it = matrix ncols $ LL.fromList $ crds
    where crds = reverse $ itfoldl' go [] it
          go xs atom = if cond atom
                       then (reverse (getter atom)) ++ xs
                       else xs

positions :: (Iterable a Atom, LL.ListLike v Scalar) => a -> M v
positions = selectAtom (const True) (\atom -> let Vector3 x y z = coord atom in [x,y,z]) 3

atomNames :: (Iterable a Atom, LL.ListLike vx String) => a -> M vx
atomNames = selectAtom (const True) (LL.singleton . BS.unpack . atName) 1

atomSerialNo :: (Iterable a Atom, LL.ListLike vx Int) => a -> M vx
atomSerialNo = selectAtom (const True) (LL.singleton . atSerial) 1

atomId :: (Iterable a Atom, LL.ListLike vx Int) => a -> M vx
atomId = mapMatrix (\x -> x-1) . atomSerialNo

elements :: (Iterable a Atom, LL.ListLike vx String) => a -> M vx
elements = selectAtom (const True) (LL.singleton . BS.unpack . element) 1

-- -- N Ca C O
-- phi = [
-- psi = []

test = do
  Just ala <- parse pdb_path
  let ns :: M [String] = atomNames ala
      es :: M [String] = elements ala
      ss :: M [Int]    = atomSerialNo ala
      ps :: M (V.Vector (String, Int)) = zipMatrix ns ss
  return ps
