{-# LANGUAGE ParallelListComp, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Textures
-- Copyright   :  Tobias Bexelius
-- License     :  BSD3
--
-- Maintainer  :  Tobias BexeliusBSD3
-- Stability   :  Experimental
-- Portability :  Portable
--
-- |
--
-----------------------------------------------------------------------------

module Textures (
    Texture3D(),
    Texture2D(),
    Texture1D(),
    TextureCube(),
    Texture(type TextureFormat, type TextureSize, type TextureVertexCoord, type TextureFragmentCoord, textureCPUFormatByteSize, sample, sampleBias, sampleLod),
    newTexture,
    newDepthTexture,
    FromFrameBufferColor(..),
    FromFrameBufferDepth(..),
    DepthColorFormat(),
    fromFrameBufferCubeColor,
    fromFrameBufferCubeDepth
) where

import Data.Vec ((:.)(..), Vec2, Vec3, Vec4)
import Shader
import Resources
import OutputMerger
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=), get)
import qualified Graphics.UI.GLUT as GLUT
import System.IO.Unsafe (unsafePerformIO)
import Formats
import Control.Monad
import Data.List


-- | A 3D texture. May only be created from main memory in GPipe.
-- 'Texture3D' @f@ has the following associated types in its 'Texture' instance:
--
-- [@TextureFormat (Texture3D f)@] @f@
--
-- [@TextureSize (Texture3D f)@] 'Vec3' 'Int'
--
-- [@TextureVertexCoord (Texture3D f)@] 'Vec3' @(@'Vertex' 'Float'@)@
--
-- [@TextureFragmentCoord (Texture3D f)@] 'Vec3' @(@'Fragment' 'Float'@)@
--  
newtype Texture3D f = Texture3D WinMappedTexture
-- | A 2D texture.
-- 'Texture2D' @f@ has the following associated types in its 'Texture' instance:
--
-- [@TextureFormat (Texture2D f)@] @f@
--
-- [@TextureSize (Texture2D f)@] 'Vec2' 'Int'
--
-- [@TextureVertexCoord (Texture2D f)@] 'Vec2' @(@'Vertex' 'Float'@)@
--
-- [@TextureFragmentCoord (Texture2D f)@] 'Vec2' @(@'Fragment' 'Float'@)@
--  
newtype Texture2D f = Texture2D WinMappedTexture
-- | A 1D texture. Assumes a frame buffer of height 1 when created from such.
-- 'Texture1D' @f@ has the following associated types in its 'Texture' instance:
--
-- [@TextureFormat (Texture1D f)@] @f@
--
-- [@TextureSize (Texture1D f)@] 'Int'
--
-- [@TextureVertexCoord (Texture1D f)@] 'Vertex' 'Float'
--
-- [@TextureFragmentCoord (Texture1D f)@] 'Fragment' 'Float'
--  
newtype Texture1D f = Texture1D WinMappedTexture
-- | A cube texture. The sides of the cube are always specified in this order: Positive X, negative X,
-- positive Y, negative Y, positive Z, negative Z.
-- 'TextureCube' @f@ has the following associated types in its 'Texture' instance:
--
-- [@TextureFormat (TextureCube f)@] @f@
--
-- [@TextureSize (TextureCube f)@] 'Vec2' 'Int' (The size of each side)
--
-- [@TextureVertexCoord (TextureCube f)@] 'Vec3' @(@'Vertex' 'Float'@)@
--
-- [@TextureFragmentCoord (TextureCube f)@] 'Vec3' @(@'Fragment' 'Float'@)@
--  
newtype TextureCube f = TextureCube WinMappedTexture

class Texture t where
    -- | The color format of the texture, affects the type of the samples from the texture. 
    type TextureFormat t
    -- | The type that is used for the dimension of texture. 
    type TextureSize t
    -- | The sample coordinate in 'Vertex's.
    type TextureVertexCoord t
    -- | The sample coordinate in 'Fragment's.
    type TextureFragmentCoord t
    mkTexture :: CPUFormat (TextureFormat t) -> GL.PixelInternalFormat -> TextureSize t -> [Ptr a] -> IO t
    -- | Calculates the byte size of all mipmaps for a specific format and size, which eases the useage of
    -- 'newTexture' and 'newDepthTexture'.
    textureCPUFormatByteSize :: CPUFormat (TextureFormat t) -> TextureSize t -> [Int]
    -- | Samples the texture using mipmaps in a 'Fragment'. 
    sample :: Sampler -> t -> TextureFragmentCoord t -> Color (TextureFormat t) (Fragment Float)
    -- | Samples the texture using mipmaps in a 'Fragment', with a bias to add to the mipmap level. 
    sampleBias :: Sampler -> t -> TextureFragmentCoord t -> Fragment Float -> Color (TextureFormat t) (Fragment Float)
    -- | Samples the texture using a specific mipmap in a 'Vertex'. 
    sampleLod :: Sampler -> t -> TextureVertexCoord t -> Vertex Float -> Color (TextureFormat t) (Vertex Float)

-- | Creates a texture from color data in main memory. It lives in the IO monad for the sake of the Ptr's, and could otherwise safely be wrapped in @unsafePerformIO@ calls.  
newTexture :: (Texture t, GPUFormat (TextureFormat t))
           => CPUFormat (TextureFormat t) -- ^ The format of the data in the provided Ptr's.
           -> TextureFormat t  -- ^ The format of the resulting texture on the GPU.
           -> TextureSize t -- ^ The dimension of the texture.
           -> [Ptr a] -- ^ A list of Ptr's for each mipmap of the texture (you may provide as many as you want).
                      --   For 'TextureCube', this list starts with all mipmaps of the first side, then the mipmaps
                      --   of the second, and so on. In this case all sides must have the same number of mipmaps.
                      --   All rows and depth levels are tightly packed, i.e. no padding between them and 1 byte alignment.
           -> IO t
newTexture f i = mkTexture f (toGLInternalFormat i)

-- | Creates a depth texture from data in main memory. The texture will have the type of a color format and is sampled as such, but contains depth
-- component information internally. It lives in the IO monad for the sake of the Ptr's, and could otherwise safely be wrapped in @unsafePerformIO@ calls.  
newDepthTexture :: (Texture t, DepthColorFormat (TextureFormat t))
                => CPUFormat (TextureFormat t) -- ^ The format of the data in the provided Ptr's.
                -> DepthFormat -- ^ The depth format of the resulting texture on the GPU.
                -> TextureSize t-- ^ The dimension of the texture.
                -> [Ptr a] -- ^ A list of Ptr's for each mipmap of the texture (you may provide as many as you want).
                           --   For 'TextureCube', this list starts with all mipmaps of the first side, then the mipmaps
                           --   of the second, and so on. In this case all sides must have the same number of mipmaps.
                           --   All rows and depth levels are tightly packed, i.e. no padding between them and 1 byte alignment.
                -> IO t
newDepthTexture f i = mkTexture f (toGLInternalFormat i)

mipLevels 1 = 1 : mipLevels 1
mipLevels x = x : mipLevels (x `div` 2)
mipLevels' 1 = [1]
mipLevels' x = x : mipLevels' (x `div` 2)
    
instance ColorFormat f => Texture (Texture3D f) where
    type TextureFormat (Texture3D f) = f
    type TextureSize (Texture3D f) = Vec3 Int 
    type TextureVertexCoord (Texture3D f) = Vec3 (Vertex Float)
    type TextureFragmentCoord (Texture3D f) = Vec3 (Fragment Float)
    mkTexture f i s ps = liftM Texture3D $ newWinMappedTexture $ \ tex cache -> 
           do f' <- evaluateDeep f
              i' <- evaluateDeep i
              x:.y:.z:.() <- evaluateDeep s
              ps' <- mapM evaluatePtr ps
              GLUT.currentWindow $= Just (contextWindow cache)
              let size = GL.TextureSize3D (fromIntegral x) (fromIntegral y) (fromIntegral z)
              GL.textureBinding GL.Texture3D $= Just tex
              mapM_ (\(n, p) -> 
                GL.texImage3D GL.Texture3D GL.NoProxy n i' size 0
                (GL.PixelData (toGLPixelFormat (undefined::f)) (toGLDataType f') p))
               [(i,p) | i<- [0..] | p<- ps']
              GL.textureLevelRange GL.Texture3D $= (0, fromIntegral $ length ps' - 1)
    textureCPUFormatByteSize f (x:.y:.z:.()) = map (\(x,y,z)-> y*z*formatRowByteSize f x) [(x',y',z') | x' <- mipLevels x | y' <- mipLevels y | z' <- mipLevels z | _ <- mipLevels' (max x (max y z))]
    sample s (Texture3D t) v = sampleBinFunc "texture3D" Sampler3D s t v
    sampleBias s (Texture3D t) v b = sampleTernFunc "texture3D" Sampler3D s t v b
    sampleLod s (Texture3D t) v m = sampleTernFunc "texture3DLod" Sampler3D s t v m
instance ColorFormat f => Texture (Texture2D f) where
    type TextureFormat (Texture2D f) = f
    type TextureSize (Texture2D f) = Vec2 Int
    type TextureVertexCoord (Texture2D f) = Vec2 (Vertex Float)
    type TextureFragmentCoord (Texture2D f) = Vec2 (Fragment Float)
    mkTexture f i s ps = liftM Texture2D $ newWinMappedTexture $ \ tex cache-> 
           do f' <- evaluateDeep f
              i' <- evaluateDeep i
              x:.y:.() <- evaluateDeep s
              ps' <- mapM evaluatePtr ps
              GLUT.currentWindow $= Just (contextWindow cache)
              let size = GL.TextureSize2D (fromIntegral x) (fromIntegral y)
              GL.textureBinding GL.Texture2D $= Just tex
              mapM_ (\(n, p) -> 
                GL.texImage2D GL.Texture2D GL.NoProxy n i' size 0
                (GL.PixelData (toGLPixelFormat (undefined::f)) (toGLDataType f') p))
               [(i,p) | i<- [0..] | p<- ps']
              GL.textureLevelRange GL.Texture2D $= (0, fromIntegral $ length ps' - 1)
    textureCPUFormatByteSize f (x:.y:.()) = map (\(x,y)-> y*formatRowByteSize f x) [(x',y') | x' <- mipLevels x | y' <- mipLevels y | _ <- mipLevels' (max x y)]
    sample s (Texture2D t) v = sampleBinFunc "texture2D" Sampler2D s t v
    sampleBias s (Texture2D t) v b = sampleTernFunc "texture2D" Sampler2D s t v b
    sampleLod s (Texture2D t) v m = sampleTernFunc "texture2DLod" Sampler2D s t v m
instance ColorFormat f => Texture (Texture1D f) where
    type TextureFormat (Texture1D f) = f
    type TextureSize (Texture1D f) = Int
    type TextureVertexCoord (Texture1D f) = Vertex Float
    type TextureFragmentCoord (Texture1D f) = Fragment Float
    mkTexture f i s ps = liftM Texture1D $ newWinMappedTexture $ \ tex cache -> 
           do f' <- evaluateDeep f
              i' <- evaluateDeep i
              x <- evaluateDeep s
              ps' <- mapM evaluatePtr ps
              GLUT.currentWindow $= Just (contextWindow cache)
              let size = GL.TextureSize1D (fromIntegral x)
              GL.textureBinding GL.Texture1D $= Just tex
              mapM_ (\(n, p) -> 
                GL.texImage1D GL.Texture1D GL.NoProxy n i' size 0
                (GL.PixelData (toGLPixelFormat (undefined::f)) (toGLDataType f') p))
               [(i,p) | i<- [0..] | p<- ps']
              GL.textureLevelRange GL.Texture1D $= (0, fromIntegral $ length ps' - 1)
    textureCPUFormatByteSize f x = map (\x-> formatRowByteSize f x) [x' | x' <- mipLevels' x]
    sample s (Texture1D t) v = sampleBinFunc "texture1D" Sampler1D s t (v:.())
    sampleBias s (Texture1D t) v b = sampleTernFunc "texture1D" Sampler1D s t (v:.()) b
    sampleLod s (Texture1D t) v m = sampleTernFunc "texture1DLod" Sampler1D s t (v:.()) m
instance ColorFormat f => Texture (TextureCube f) where
    type TextureFormat (TextureCube f) = f
    type TextureSize (TextureCube f) = Vec2 Int
    type TextureVertexCoord (TextureCube f) = Vec3 (Vertex Float)
    type TextureFragmentCoord (TextureCube f) = Vec3 (Fragment Float)
    mkTexture f i s ps = liftM TextureCube $ newWinMappedTexture $ \ tex cache -> 
           do f' <- evaluateDeep f
              i' <- evaluateDeep i
              x:.y:.() <- evaluateDeep s
              ps' <- mapM evaluatePtr ps
              GLUT.currentWindow $= Just (contextWindow cache)
              let size = GL.TextureSize2D (fromIntegral x) (fromIntegral y)
              GL.textureBinding GL.TextureCubeMap $= Just tex
              mapM_
                (\(t,ps'') -> 
                  mapM_
                        (\(n, p) -> 
                          GL.texImage2D t GL.NoProxy n i' size 0
                          (GL.PixelData (toGLPixelFormat (undefined::f)) (toGLDataType f') p))
                        [(i,p) | i<- [0..] | p<- ps''])
                  [(t,ps'') | t <- cubeMapTargets | ps'' <- splitIn 6 ps']
              GL.textureLevelRange GL.TextureCubeMap $= (0, fromIntegral $ length ps' - 1)
    textureCPUFormatByteSize f (x:.y:.()) = concat $ replicate 6 $ map (\(x,y)-> y*formatRowByteSize f x) [(x',y') | x' <- mipLevels x | y' <- mipLevels y | _ <- mipLevels' (max x y)]
    sample s (TextureCube t) v = sampleBinFunc "textureCube" Sampler3D s t v
    sampleBias s (TextureCube t) v b = sampleTernFunc "textureCube" Sampler3D s t v b
    sampleLod s (TextureCube t) v m = sampleTernFunc "textureCubeLod" Sampler3D s t v m

-- | The formats that is instances of this class may be used as depth textures, i.e. created with
--   'newDepthTexture', 'fromFrameBufferDepth' and 'fromFrameBufferCubeDepth'.
class ColorFormat a => DepthColorFormat a
instance DepthColorFormat LuminanceFormat
instance DepthColorFormat AlphaFormat

-- | The textures that is instances of this class may be created from a 'FrameBuffer's color buffer.
class (Texture t) => FromFrameBufferColor t c where
    -- | Create a texture of a specific format from a 'FrameBuffer' and a size. 
    fromFrameBufferColor :: TextureFormat t -> TextureSize t -> FrameBuffer c d s -> t   
  
instance ColorFormat f => FromFrameBufferColor (Texture2D f) f where
    fromFrameBufferColor f s fb = Texture2D $ unsafePerformIO $ do
         newWinMappedTexture $ \ tex cache -> 
               do f' <- evaluateDeep (toGLInternalFormat f)
                  x:.y:.() <- evaluateDeep s
                  let size = GL.TextureSize2D (fromIntegral x) (fromIntegral y)
                  runFrameBufferInContext cache s fb
                  GL.textureBinding GL.Texture2D $= Just tex
                  GL.copyTexImage2D GL.Texture2D 0 f' (GL.Position 0 0) size 0
                  GL.textureLevelRange GL.Texture2D $= (0, 0)
instance ColorFormat f => FromFrameBufferColor (Texture1D f) f where
    fromFrameBufferColor f s fb = Texture1D $ unsafePerformIO $ do
         newWinMappedTexture $ \ tex cache -> 
               do f' <- evaluateDeep (toGLInternalFormat f)
                  x <- evaluateDeep s
                  let size = GL.TextureSize1D (fromIntegral x)
                  runFrameBufferInContext cache (x:.1:.()) fb
                  GL.textureBinding GL.Texture1D $= Just tex
                  GL.copyTexImage1D GL.Texture1D 0 f' (GL.Position 0 0) size 0
                  GL.textureLevelRange GL.Texture1D $= (0, 0)

-- | The textures that is instances of this class may be created from a 'FrameBuffer's depth buffer.
-- The texture will have the type of a color format and is sampled as such, but contains depth
-- component information internally.
class Texture t => FromFrameBufferDepth t where
    -- | Create a texture of a specific depth format from a 'FrameBuffer' and a size.
    fromFrameBufferDepth :: DepthFormat -> TextureSize t -> FrameBuffer c DepthFormat s -> t
   
instance DepthColorFormat f => FromFrameBufferDepth (Texture2D f) where
    fromFrameBufferDepth f s fb = Texture2D $ unsafePerformIO $ do
         newWinMappedTexture $ \ tex cache -> 
               do f' <- evaluateDeep (toGLInternalFormat f)
                  x:.y:.() <- evaluateDeep s
                  let size = GL.TextureSize2D (fromIntegral x) (fromIntegral y)
                  runFrameBufferInContext cache s fb
                  GL.textureBinding GL.Texture2D $= Just tex
                  GL.copyTexImage2D GL.Texture2D 0 f' (GL.Position 0 0) size 0
                  GL.textureLevelRange GL.Texture2D $= (0, 0)
instance DepthColorFormat f => FromFrameBufferDepth (Texture1D f) where
    fromFrameBufferDepth f s fb = Texture1D $ unsafePerformIO $ do
         newWinMappedTexture $ \ tex cache -> 
               do f' <- evaluateDeep (toGLInternalFormat f)
                  x <- evaluateDeep s
                  let size = GL.TextureSize1D (fromIntegral x)
                  runFrameBufferInContext cache (x:.1:.()) fb
                  GL.textureBinding GL.Texture1D $= Just tex
                  GL.copyTexImage1D GL.Texture1D 0 f' (GL.Position 0 0) size 0
                  GL.textureLevelRange GL.Texture1D $= (0, 0)

-- | Create a 'TextureCube' of a specific format and size from the the color buffers of six framebuffers.
fromFrameBufferCubeColor :: ColorFormat c => c -> Vec2 Int -> FrameBuffer c d1 s1 -> FrameBuffer c d2 s2 -> FrameBuffer c d3 s3 -> FrameBuffer c d4 s4 -> FrameBuffer c d5 s5 -> FrameBuffer c d6 s6 -> TextureCube c
-- | Create a 'TextureCube' of a specific depth format and size from the the depth buffers of six framebuffers.
-- The texture will have the type of a color format and is sampled as such, but contains depth
-- component information internally.
fromFrameBufferCubeDepth :: DepthColorFormat d => DepthFormat -> Vec2 Int -> FrameBuffer c1 DepthFormat s1 -> FrameBuffer c2 DepthFormat s2 -> FrameBuffer c3 DepthFormat s3 -> FrameBuffer c4 DepthFormat s4 -> FrameBuffer c5 DepthFormat s5 -> FrameBuffer c6 DepthFormat s6 -> TextureCube d

fromFrameBufferCubeColor f s b0 b1 b2 b3 b4 b5 = TextureCube $ unsafePerformIO $ do
         newWinMappedTexture $ \ tex cache -> 
               do f' <- evaluateDeep (toGLInternalFormat f)
                  x:.y:.() <- evaluateDeep s
                  let size = GL.TextureSize2D (fromIntegral x) (fromIntegral y)
                  mapM_ (\ (t,io)-> do
                                 io
                                 GL.textureBinding GL.TextureCubeMap $= Just tex
                                 GL.copyTexImage2D t 0 f' (GL.Position 0 0) size 0)
                        [(t,io) | t <- cubeMapTargets | io <- [runFrameBufferInContext cache s b0,
                                                               runFrameBufferInContext cache s b1,
                                                               runFrameBufferInContext cache s b2,
                                                               runFrameBufferInContext cache s b3,
                                                               runFrameBufferInContext cache s b4,
                                                               runFrameBufferInContext cache s b5]]
                  GL.textureLevelRange GL.TextureCubeMap $= (0, 0)

fromFrameBufferCubeDepth f s b0 b1 b2 b3 b4 b5 = TextureCube $ unsafePerformIO $ do
         newWinMappedTexture $ \ tex cache -> 
               do f' <- evaluateDeep (toGLInternalFormat f)
                  x:.y:.() <- evaluateDeep s
                  let size = GL.TextureSize2D (fromIntegral x) (fromIntegral y)
                  mapM_ (\ (t,io)-> do
                                 io
                                 GL.textureBinding GL.TextureCubeMap $= Just tex
                                 GL.copyTexImage2D t 0 f' (GL.Position 0 0) size 0)
                        [(t,io) | t <- cubeMapTargets | io <- [runFrameBufferInContext cache s b0,
                                                               runFrameBufferInContext cache s b1,
                                                               runFrameBufferInContext cache s b2,
                                                               runFrameBufferInContext cache s b3,
                                                               runFrameBufferInContext cache s b4,
                                                               runFrameBufferInContext cache s b5]]
                  GL.textureLevelRange GL.TextureCubeMap $= (0, 0)

splitIn n xs = unfoldr f xs
                where f [] = Nothing
                      f ys = Just $ splitAt (length xs `div` n) ys
