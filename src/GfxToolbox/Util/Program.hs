{-
 - Program.hs
 - By Steven Smith
 -}

module GfxToolbox.Util.Program where

import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.ByteString (ByteString)
import Graphics.Rendering.OpenGL

import GfxToolbox.Util.Error

import qualified Data.ByteString as B

mkProgram :: [(ByteString, ShaderType)] -> EitherT String IO Program
mkProgram [] = left "No shader source files given"
mkProgram ss = do
    program <- lift createProgram
    shaders <- mapM (uncurry mkShader) ss
    status <- lift $ do
        attachedShaders program $= shaders
        linkProgram program
        get $ linkStatus program
    onFalse status $ do
        err <- get $ programInfoLog program
        return ("Error during shader linking:\n" ++ err)
    lift $ currentProgram $= Just program
    return program

mkShader :: ByteString -> ShaderType -> EitherT String IO Shader
mkShader src ty = do
    shader <- lift $ createShader ty
    status <- lift $ do
        shaderSourceBS shader $= src
        compileShader shader
        get $ compileStatus shader
    onFalse status $ do
        err <- get $ shaderInfoLog shader
        return ("Error during shader compilation (" ++ show ty ++ "):\n" ++ err)
    return shader
