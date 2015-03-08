{-
 - Mode2.hs
 - By Steven Smith
 -}

module GfxToolbox.Mode2 where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Vector.Storable (unsafeWith)
import Foreign.Storable
import Graphics.Rendering.OpenGL

import GfxToolbox.Util.Attributes
import GfxToolbox.Util.Buffer
import GfxToolbox.Util.Error
import GfxToolbox.Util.Image
import GfxToolbox.Util.Program
import GfxToolbox.Util.Storable
import GfxToolbox.Util.VertexObjects

data SpriteProgram = SpriteProgram {
    sProgram :: Program,
    sVao :: VertexArrayObject,
    sVbo :: BufferObject,
    sScreenSizeUni :: UniformLocation,
    sWindowSizeUni :: UniformLocation,
    sTexSamplerUni :: UniformLocation,
    sCameraOffsetUni :: UniformLocation
}

data Sprite = Sprite {
    spritePos :: Vector2 GLint,
    spriteTexPos :: Vector2 GLushort,
    spriteSize :: Vector2 GLushort,
    spriteTex :: GLushort
}

-- Placeholders for undefined field markers

fSpritePos :: Vector2 GLint
fSpritePos = undefined

fSpriteTexPos :: Vector2 GLushort
fSpriteTexPos = undefined

fSpriteSize :: Vector2 GLushort
fSpriteSize = undefined

fSpriteTex :: GLushort
fSpriteTex = undefined

instance Storable Sprite where
    sizeOf _ = sizeOf fSpritePos
             + sizeOf fSpriteTexPos
             + sizeOf fSpriteSize
             + sizeOf fSpriteTex
    alignment _ = alignment fSpritePos
    peek ptr = do
        (pos,  ptr1) <- peekField fSpritePos ptr
        (tpos, ptr2) <- peekField fSpriteTexPos ptr1
        (siz,  ptr3) <- peekField fSpriteSize ptr2
        (tex,  _)    <- peekField fSpriteTex ptr3
        return $ Sprite pos tpos siz tex
    poke ptr (Sprite pos tpos siz tex) = do
        ptr1 <- pokeField pos ptr
        ptr2 <- pokeField tpos ptr1
        ptr3 <- pokeField siz ptr2
        _    <- pokeField tex ptr3
        return ()

instance HasAttributes Sprite where
    attributes _ =
        [ ("spritePos",    getAttribute fSpritePos)
        , ("spriteTexPos", getAttribute fSpriteTexPos)
        , ("spriteSize",   getAttribute fSpriteSize)
        , ("spriteTex",    getAttribute fSpriteTex)
        ]

-- Drawing related things

setScreenSize :: SpriteProgram -> Int -> Int -> IO ()
setScreenSize sp w h = 
    uniform (sScreenSizeUni sp) $= fromI <$> Vertex2 w h
  where
    fromI :: Integral a => a -> GLuint
    fromI = fromIntegral

setWindowSize :: SpriteProgram -> Int -> Int -> IO ()
setWindowSize sp w h =
    uniform (sWindowSizeUni sp) $= fromI <$> Vertex2 w h
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
    unfirom (sTexSamplerUni sp) $= tu

drawSprites :: Buffer Sprite -> IO ()
drawSprites buf = unsafeWithBufferData buf go
  where
    go ptr count = do
        let size = fromIntegral $ count * (sizeOf (undefined :: Sprite))
        bufferData ArrayBuffer $= (size, ptr, DynamicDraw)
        drawArrays Points 0 (fromIntegral count)

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
