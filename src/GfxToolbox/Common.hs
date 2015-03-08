{-
 - Common.hs
 - By Steven Smith
 -}

module GfxToolbox.Common where

import Control.Exception (bracket)
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import GfxToolbox.Util.Error

import Prelude hiding (init)

errorCallback :: ErrorCallback
errorCallback e s =
    putStrLn $ "GLFW error: {" ++ show e ++ "}, message: \"" ++ s ++ "\""

initialize :: Int -> Int -> String -> (Window -> EitherT String IO ()) -> IO ()
initialize w h s a = do
    setErrorCallback $ Just errorCallback
    result <- bracket init (const terminate) (runEitherT . start w h a s)
    either putStrLn (const $ return ()) result
  where
    start _ _ _ _ False = left "Failed to initialized GLFW"
    start w h action s True = do
        window <- lift $ do
            defaultWindowHints
            windowHint $ WindowHint'ContextVersionMajor 3
            windowHint $ WindowHint'ContextVersionMinor 2
            windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
            windowHint $ WindowHint'OpenGLForwardCompat True
            window <- createWindow w h s Nothing Nothing
            makeContextCurrent window
            return window
        window' <- guardMaybe window "valid window"
        action window'

simpleLoop :: a -> (a -> IO a) -> Window -> EitherT String IO ()
simpleLoop a f window = loopUntil (lift $ windowShouldClose window) a go
  where
    go a = lift $ do
        clear [ColorBuffer, DepthBuffer]
        a' <- f a
        swapBuffers window
        pollEvents
        return a'
    loopUntil m a f = do
        cond <- m
        if cond then return () else do
            a' <- f a
            loopUntil m a' f
