{-
 - Main.hs
 - By Steven Smith
 -}

module Main where

import Control.Monad.Trans
import Control.Monad.Trans.Either
import Graphics.UI.GLFW hiding (setWindowSize)
import Graphics.Rendering.OpenGL

import GfxToolbox.Common
import GfxToolbox.Mode0
import GfxToolbox.Util.Buffer

main :: IO ()
main = initialize 432 240 "Test Game" preloop

preloop :: Window -> EitherT String IO ()
preloop w = do
    -- Make buffer
    buffer <- lift mkBuffer
    -- Make sprite program
    program <- mkSpriteProgram
    -- Load sprite sheet
    bgTex <- loadSpriteSheets 0 ["sprites/background1.png",
                                 "sprites/background2.png"]
    charTex <- loadSpriteSheets 1 ["sprites/lastguardian_all.png"]
    -- Set uniforms
    lift $ setScreenSize program 432 240
    -- Enter game loop
    simpleLoop () (drawLoop buffer program bgTex charTex) w

drawLoop :: Buffer Sprite -> SpriteProgram -> TextureUnit -> TextureUnit -> () -> IO ()
drawLoop buf program bgTex charTex _ = do
    -- Background
    clearBuffer buf
    pushValue buf $ Sprite (Vector2 64 64) (Vector2 128 160) (Vector2 32 32) 0
    pushValue buf $ Sprite (Vector2 128 64) (Vector2 320 96) (Vector2 64 64) 1
    setTextureSampler program bgTex
    drawSprites buf
    -- Character
    clearBuffer buf
    pushValue buf $ Sprite (Vector2 32 64) (Vector2 0 0) (Vector2 32 32) 0
    setTextureSampler program charTex
    drawSprites buf
