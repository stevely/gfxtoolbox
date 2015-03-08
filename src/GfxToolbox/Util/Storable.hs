{-
 - Storable.hs
 - By Steven Smith
 -}

module GfxToolbox.Util.Storable where

import Foreign.Ptr
import Foreign.Storable

asPtrType :: b -> Ptr a -> Ptr b
asPtrType _ = castPtr

peekField :: Storable a => a -> Ptr x -> IO (a, Ptr a)
peekField und ptr = do
    a <- peek (asPtrType und ptr)
    return (a, ptr `plusPtr` sizeOf und)

pokeField :: Storable a => a -> Ptr x -> IO (Ptr a)
pokeField a ptr = do
    poke (asPtrType a ptr) a
    return $ ptr `plusPtr` sizeOf a
