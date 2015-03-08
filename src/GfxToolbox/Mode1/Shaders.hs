{-
 - Shaders.hs
 - Mode 1 shaders
 - By Steven Smith
 -}

{-# LANGUAGE OverloadedStrings #-}

module GfxToolbox.Mode1.Shaders where

import Data.ByteString (ByteString)
import Data.List (intersperse)
import Graphics.Rendering.OpenGL

import qualified Data.ByteString as B

vertexShader :: (ByteString, ShaderType)
vertexShader = (B.concat . intersperse "\n" $ s, VertexShader)
  where
    s = [ "#version 150 core"
        , ""
        , "in ivec2  spritePos;"
        , "in uvec2 spriteTexPos;"
        , "in uvec2 spriteSize;"
        , "in uint  spriteTex;"
        , ""
        , "out uvec2 vertTexPos;"
        , "out uvec2 vertSize;"
        , "out uint vertTex;"
        , ""
        , "uniform uvec2 screenSize;"
        , "uniform ivec2 cameraOffset;"
        , ""
        , "void main() {"
        , "    vec2 pos = spritePos - cameraOffset;"
        , "    gl_Position = vec4((2.0 * (pos.x / float(screenSize.x))) - 1.0,"
        , "                       (2.0 * (pos.y / float(screenSize.y))) - 1.0,"
        , "                        0,"
        , "                        1);"
        , "    vertTexPos = spriteTexPos;"
        , "    vertSize = spriteSize;"
        , "    vertTex = spriteTex;"
        , "}" ]

geometryShader :: (ByteString, ShaderType)
geometryShader = (B.concat . intersperse "\n" $ s, GeometryShader)
  where
    s = [ "#version 150 core"
        , ""
        , "layout(points) in;"
        , "layout(triangle_strip, max_vertices=4) out;"
        , ""
        , "in uvec2 vertTexPos[];"
        , "in uvec2 vertSize[];"
        , "in uint  vertTex[];"
        , ""
        , "out vec2 fragTexPos;"
        , "flat out uint fragTex;"
        , ""
        , "uniform uvec2 screenSize;"
        , ""
        , "void main() {"
        , "    vec4 pos = gl_in[0].gl_Position;"
        , "    uvec2 texPos = vertTexPos[0];"
        , "    uvec2 size = vertSize[0];"
        , "    vec2 adjustedSize = vec2(size) / vec2(int(screenSize.x / 2U)"
        , "                                         ,int(screenSize.y / 2U));"
        , ""
        , "    // Original point (0,0)"
        , "    gl_Position = pos;"
        , "    fragTexPos = vec2(texPos);"
        , "    fragTex = vertTex[0];"
        , "    EmitVertex();"
        , ""
        , "    // First new point (1,0)"
        , "    gl_Position = vec4(pos.x + adjustedSize.x"
        , "                      ,pos.y"
        , "                      ,pos.z"
        , "                      ,pos.w);"
        , "    fragTexPos = vec2(texPos.x + size.x"
        , "                      ,texPos.y);"
        , "    fragTex = vertTex[0];"
        , "    EmitVertex();"
        , ""
        , "    // Second new point (0,1)"
        , "    gl_Position = vec4(pos.x"
        , "                      ,pos.y + adjustedSize.y"
        , "                      ,pos.z"
        , "                      ,pos.w);"
        , "    fragTexPos = vec2(texPos.x"
        , "                      ,texPos.y + size.y);"
        , "    fragTex = vertTex[0];"
        , "    EmitVertex();"
        , ""
        , "    // Third new point (1,1)"
        , "    gl_Position = vec4(pos.x + adjustedSize.x"
        , "                      ,pos.y + adjustedSize.y"
        , "                      ,pos.z"
        , "                      ,pos.w);"
        , "    fragTexPos = vec2(texPos.x + size.x"
        , "                      ,texPos.y + size.y);"
        , "    fragTex = vertTex[0];"
        , "    EmitVertex();"
        , ""
        , "    // Point turned into quad"
        , "    EndPrimitive();"
        , "}" ]

fragmentShader :: (ByteString, ShaderType)
fragmentShader = (B.concat . intersperse "\n" $ s, FragmentShader)
  where
    s = [ "#version 150 core"
        , ""
        , "in vec2 fragTexPos;"
        , "flat in uint fragTex;"
        , ""
        , "out vec3 color;"
        , ""
        , "uniform sampler2DArray texSampler;"
        , ""
        , "void main() {"
        , "    // (0,0) in UV coords is top-left, in our app (0,0) is bottom-left"
        , "    // Have to invert Y-axis to fix this"
        , "    ivec2 iUV = ivec2(fragTexPos);"
        , "    int layer = int(fragTex);"
        , "    int fixedV = textureSize(texSampler, 0).t - iUV.t;"
        , "    vec4 texel = texelFetch(texSampler, ivec3(iUV.s, fixedV, layer), 0);"
        , "    if( texel.a < 0.5 ) {"
        , "        discard;"
        , "    }"
        , "    color = texel.rgb;"
        , "}" ]
