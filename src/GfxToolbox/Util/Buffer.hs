{-
 - Buffer.hs
 - By Steven Smith
 -}

module GfxToolbox.Util.Buffer
( Buffer
, mkBuffer
, mkBufferWithSize
, clearBuffer
, pushValue
, pushValues
, unsafeWithBufferData
)
where

import Prelude hiding (length)

import Control.Applicative
import Control.Concurrent.MVar
import Data.Foldable
import Data.Vector.Storable.Mutable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

newtype Buffer a = Buffer (MVar (IOVector a, Int))

mkBuffer :: Storable a => IO (Buffer a)
mkBuffer = mkBufferWithSize 128

mkBufferWithSize :: Storable a => Int -> IO (Buffer a)
mkBufferWithSize n = Buffer <$> (go >>= newMVar)
  where
    go = (,) <$> new n <*> pure 0

clearBuffer :: Storable a => Buffer a -> IO ()
clearBuffer (Buffer mvar) = modifyMVar_ mvar go
  where
    go (v,_) = return (v,0)

pushValue :: Storable a => Buffer a -> a -> IO ()
pushValue (Buffer mvar) x = modifyMVar_ mvar go
  where
    go vc = writeToVec vc x

pushValues :: (Storable a, Foldable t) => Buffer a -> t a -> IO ()
pushValues (Buffer mvar) xs = modifyMVar_ mvar go
  where
    go vc = foldlM writeToVec vc xs

writeToVec :: Storable a => (IOVector a, Int) -> a -> IO (IOVector a, Int)
writeToVec (v,c) x =
    if c == length v then do
        v' <- grow v (c * 2)
        write v' c x
        return (v', c + 1)
    else do
        write v c x
        return (v, c + 1)

unsafeWithBufferData :: Storable a => Buffer a -> (Ptr a -> Int -> IO ()) -> IO ()
unsafeWithBufferData (Buffer mvar) fn = withMVar mvar go
  where
    go (v,c) = unsafeWith v (\ptr -> fn ptr c)
