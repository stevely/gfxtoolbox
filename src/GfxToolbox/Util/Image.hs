{-
 - Image.hs
 - By Steven Smith
 -}

module GfxToolbox.Util.Image where

import Codec.Picture
import Codec.Picture.Types (convertImage)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Function (on)
import Data.List (intersperse)
import Foreign.Ptr
import Graphics.Rendering.OpenGL
import Data.Vector.Storable (unsafeWith)

import GfxToolbox.Util.Error

loadTextureArray :: [FilePath] -> GLuint -> EitherT String IO TextureUnit
loadTextureArray [] _ = left "No sprite files given"
loadTextureArray fps tex = do
    texObject <- lift genObjectName
    lift $ activeTexture $= texUnit
    lift $ textureBinding Texture2DArray $= Just texObject
    loadImages fps
    lift $ textureFilter Texture2DArray $= ((Nearest, Nothing), Nearest)
    errs <- lift $ get errors
    guardBool (null errs) $
        concat (intersperse "\n" ("Errors loading textures:" : map show errs))
    return texUnit
  where
    texUnit = TextureUnit tex

type ImageData = (Int, Int, PixelInternalFormat, PixelFormat, DataType)

loadImages :: [FilePath] -> EitherT String IO ()
loadImages [] = left "No sprite files given"
loadImages (fp:fps) = do
    fmt <- loadImageAsTexture (lift . createTexture) 0 fp
    zipWithM_ (loadImageAsTexture (checkEq fmt)) [1..] fps
  where
    createTexture (w, h, pif, pf, dt) = texImage3D Texture2DArray NoProxy 0 pif
        (on TextureSize3D fromIntegral w h $ fromIntegral (length fps + 1)) 0
        (PixelData pf dt nullPtr)
    checkEq fmt1 fmt2
        | fmt1 == fmt2 = return ()
        | otherwise = left "Sprite image properties don't match"

loadImageAsTexture :: (ImageData -> EitherT String IO ()) -> Int -> FilePath
                   -> EitherT String IO ImageData
loadImageAsTexture action i fp = do
    image <- EitherT $ readImage fp
    switch image
  where
    switch image = case image of
        ImageY8     (Image w h dat) ->
            go dat (w, h, Luminance8, Luminance, UnsignedByte)
        ImageY16    (Image w h dat) ->
            go dat (w, h, Luminance16, Luminance, UnsignedShort)
        ImageYF     (Image w h dat) ->
            go dat (w, h, Luminance', Luminance, Float)
        ImageYA8    (Image w h dat) ->
            go dat (w, h, Luminance8Alpha8, LuminanceAlpha, UnsignedByte)
        ImageYA16   (Image w h dat) ->
            go dat (w, h, Luminance16Alpha16, LuminanceAlpha, UnsignedShort)
        ImageRGB8   (Image w h dat) -> go dat (w, h, RGB8, RGB, UnsignedByte)
        ImageRGB16  (Image w h dat) -> go dat (w, h, RGB16, RGB, UnsignedShort)
        ImageRGBF   (Image w h dat) -> go dat (w, h, RGB32F, RGB, Float)
        ImageRGBA8  (Image w h dat) -> go dat (w, h, RGBA8, RGBA, UnsignedByte)
        ImageRGBA16 (Image w h dat) -> go dat (w, h, RGBA16, RGBA, UnsignedShort)
        ImageYCbCr8 image' -> switch . ImageRGB8 $ convertImage image'
        ImageCMYK8 image' -> switch . ImageRGB8 $ convertImage image'
        ImageCMYK16 image' -> switch . ImageRGB16 $ convertImage image'
    go dat idat = do
        action idat
        lift $ go' dat idat
        return idat
    go' dat (w, h, pif, pf, dt) = unsafeWith dat $ \ptr ->
        texSubImage3D Texture2DArray 0 (TexturePosition3D 0 0 (fromIntegral i))
        (on TextureSize3D fromIntegral w h $ 1) (PixelData pf dt ptr)
