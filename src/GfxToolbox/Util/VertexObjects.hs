{-
 - VertexObjects.hs
 - By Steven Smith
 -}

module GfxToolbox.Util.VertexObjects where

import Graphics.Rendering.OpenGL

mkVao :: IO VertexArrayObject
mkVao = do
    vao <- genObjectName
    bindVertexArrayObject $= Just vao
    return vao

mkVbo :: IO BufferObject
mkVbo = do
    vbo <- genObjectName
    bindBuffer ArrayBuffer $= Just vbo
    return vbo
