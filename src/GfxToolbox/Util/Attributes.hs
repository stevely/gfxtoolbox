{-
 - Attributes.hs
 - By Steven Smith
 -}

module GfxToolbox.Util.Attributes where

import Control.Monad
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL

data Attribute = Attribute {
    attIntHandling :: IntegerHandling,
    attDataType :: DataType,
    attStride :: Stride,
    attComponents :: NumComponents
}

class HasAttributes a where
    attributes :: a -> [(String, Attribute)]

class AttributeComponent a where
    getAttribute :: a -> Attribute

getSize :: (Storable a, Num b) => a -> b
getSize a = fromIntegral (sizeOf a)

setupAttributes :: (Storable a, HasAttributes a) => a -> Program -> IO ()
setupAttributes a program = foldM_ go nullPtr (attributes a)
  where
    go = setupAttribute program (fromIntegral $ sizeOf a)

setupAttribute :: Program -> Stride -> Ptr a -> (String, Attribute) -> IO (Ptr a)
setupAttribute p s ptr (name, Attribute intHand dataType stride comps) = do
    loc <- get $ attribLocation p name
    vertexAttribPointer loc $= (intHand, vad)
    vertexAttribArray loc $= Enabled
    return $ ptr `plusPtr` (fromIntegral stride)
  where
    vad = VertexArrayDescriptor comps dataType s ptr

printBytes :: Storable a => Ptr a -> Int -> IO ()
printBytes ptr n = go ptr' n'
  where
    ptr' = castPtr ptr :: Ptr CUChar
    n' = sizeOf . (undefined :: Ptr a -> a) $ ptr
    go _ 0 = putStrLn ""
    go p x = do
        c <- peek p
        putStr $ ' ' : show c
        go (p `plusPtr` sizeOf (undefined :: CUChar)) (x - 1)

-- Instance declarations. It shouldn't ever be necessary to make a new instance
-- of AttributeComponent.

instance AttributeComponent CUChar where
    getAttribute a = Attribute KeepIntegral UnsignedByte (getSize a) 1

instance AttributeComponent CSChar where
    getAttribute a = Attribute KeepIntegral Byte (getSize a) 1

instance AttributeComponent CChar where
    getAttribute a = Attribute KeepIntegral Byte (getSize a) 1

instance AttributeComponent CShort where
    getAttribute a = Attribute KeepIntegral Short (getSize a) 1

instance AttributeComponent CUShort where
    getAttribute a = Attribute KeepIntegral UnsignedShort (getSize a) 1

instance AttributeComponent CInt where
    getAttribute a = Attribute KeepIntegral Int (getSize a) 1

instance AttributeComponent CUInt where
    getAttribute a = Attribute KeepIntegral UnsignedInt (getSize a) 1

instance AttributeComponent CFloat where
    getAttribute a = Attribute ToFloat Float (getSize a) 1

instance AttributeComponent CDouble where
    getAttribute a = Attribute ToFloat Double (getSize a) 1

instance AttributeComponent a => AttributeComponent (Vector1 a) where
    getAttribute = getAttribute . (undefined :: Vector1 a -> a)

instance AttributeComponent a => AttributeComponent (Vector2 a) where
    getAttribute a = atts { attComponents = attComponents atts * 2
                          , attStride = attStride atts * 2 }
      where
        atts = getAttribute . (undefined :: Vector2 a -> a) $ a

instance AttributeComponent a => AttributeComponent (Vector3 a) where
    getAttribute a = atts { attComponents = attComponents atts * 3
                          , attStride = attStride atts * 3 }
      where
        atts = getAttribute . (undefined :: Vector3 a -> a) $ a

instance AttributeComponent a => AttributeComponent (Vector4 a) where
    getAttribute a = atts { attComponents = attComponents atts * 4
                          , attStride = attStride atts * 4 }
      where
        atts = getAttribute . (undefined :: Vector4 a -> a) $ a

instance AttributeComponent a => AttributeComponent (Vertex1 a) where
    getAttribute = getAttribute . (undefined :: Vertex1 a -> a)

instance AttributeComponent a => AttributeComponent (Vertex2 a) where
    getAttribute a = atts { attComponents = attComponents atts * 2
                          , attStride = attStride atts * 2 }
      where
        atts = getAttribute . (undefined :: Vertex2 a -> a) $ a

instance AttributeComponent a => AttributeComponent (Vertex3 a) where
    getAttribute a = atts { attComponents = attComponents atts * 3
                          , attStride = attStride atts * 3 }
      where
        atts = getAttribute . (undefined :: Vertex3 a -> a) $ a

instance AttributeComponent a => AttributeComponent (Vertex4 a) where
    getAttribute a = atts { attComponents = attComponents atts * 4
                          , attStride = attStride atts * 4 }
      where
        atts = getAttribute . (undefined :: Vertex4 a -> a) $ a
