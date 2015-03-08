{-
 - Mode1.hs
 - By Steven Smith
 -}

{-# LANGUAGE TypeFamilies #-}

module GfxToolbox.Mode1 where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Foreign.Storable
import Graphics.Rendering.OpenGL

import GfxToolbox.Mode1.Shaders
import GfxToolbox.Util.Attributes
import GfxToolbox.Util.Buffer
import GfxToolbox.Util.Error
import GfxToolbox.Util.Image
import GfxToolbox.Util.Program
import GfxToolbox.Util.Storable
import GfxToolbox.Util.VertexObjects
import Foreign.AutoStorable

data SpriteProgram = SpriteProgram {
    sProgram :: Program,
    sVao :: VertexArrayObject,
    sVbo :: BufferObject,
    sScreenSizeUni :: UniformLocation,
    sTexSamplerUni :: UniformLocation,
    sCameraOffsetUni :: UniformLocation
}

data Sprite = Sprite {
    spritePos :: Vector2 GLint,
    spriteTexPos :: Vector2 GLushort,
    spriteSize :: Vector2 GLushort,
    spriteTex :: GLushort
}

instance AutoStorable Sprite where
    type Tuple Sprite = T4 (Vector2 GLint) (Vector2 GLushort) (Vector2 GLushort) GLushort
    toTuple (Sprite pos tpos size tex) = T4 pos tpos size tex
    fromTuple (T4 pos tpos size tex) = Sprite pos tpos size tex

instance Storable Sprite where
    sizeOf = autoSizeOf
    alignment = autoAlignment
    peek = autoPeek
    poke = autoPoke

instance HasAttributes Sprite where
    attributes ~(Sprite pos tpos size tex) =
        [ ("spritePos",    getAttribute pos)
        , ("spriteTexPos", getAttribute tpos)
        , ("spriteSize",   getAttribute size)
        , ("spriteTex",    getAttribute tex)
        ]

-- Drawing related things

setScreenSize :: SpriteProgram -> Int -> Int -> IO ()
setScreenSize sp w h =
    uniform (sScreenSizeUni sp) $= fromI <$> Vertex2 w h
  where
    fromI :: Integral a => a -> GLuint
    fromI = fromIntegral

setCameraOffset :: SpriteProgram -> Int -> Int -> IO ()
setCameraOffset sp x y =
    uniform (sCameraOffsetUni sp) $= fromI <$> Vertex2 x y
  where
    fromI :: Integral a => a -> GLint
    fromI = fromIntegral

setTextureSampler :: SpriteProgram -> TextureUnit -> IO ()
setTextureSampler sp tu =
    uniform (sTexSamplerUni sp) $= tu

drawSprites :: Buffer Sprite -> IO ()
drawSprites buf = unsafeWithBufferData buf go
  where
    go ptr count = do
        let size = fromIntegral $ count * (sizeOf (undefined :: Sprite))
        bufferData ArrayBuffer $= (size, ptr, DynamicDraw)
        drawArrays Points 0 (fromIntegral count)

-- Creating sprite programs

mkSpriteProgram :: EitherT String IO SpriteProgram
mkSpriteProgram = do
    prog <- mkProgram [vertexShader, geometryShader, fragmentShader]
    lift $ do
        screenSize <- get $ uniformLocation prog "screenSize"
        texSampler <- get $ uniformLocation prog "texSampler"
        camOffset <- get $ uniformLocation prog "cameraOffset"
        vao <- mkVao
        vbo <- mkVbo
        setupAttributes (undefined :: Sprite) prog
        return $ SpriteProgram prog vao vbo screenSize texSampler camOffset

loadSpriteSheets :: [FilePath] -> GLuint -> EitherT String IO TextureUnit
loadSpriteSheets = loadTextureArray
