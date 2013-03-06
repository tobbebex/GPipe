{-# LANGUAGE Arrows, TypeOperators, TypeFamilies, FlexibleInstances, GeneralizedNewtypeDeriving, TypeSynonymInstances, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Rasterizer
-- Copyright   :  Tobias Bexelius
-- License     :  BSD3
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- |
--
-----------------------------------------------------------------------------

module Rasterizer (
    Rasterizer(),
    VertexOutput(..),
    rasterizeFront,
    rasterizeBack,
    rasterizeFrontAndBack,
) where

import Shader
import Data.Vec ((:.)(..), Vec2, Vec3, Vec4)
import GPUStream
import Data.Functor.Identity
import Control.Arrow (Arrow, returnA)
import Control.Category (Category)

-- | An arrow by which vertex data gets converted to fragment data.
--   Use 'toFragment' in the existing instances of 'VertexOutput' to operate in this arrow.
newtype Rasterizer a b = Rasterizer {fromRasterizer :: a -> b} deriving (Category, Arrow)

-- | The context of types that can be rasterized from vertices in 'PrimitiveStream's to fragments in 'FragmentStream's.
--   Create your own instances in terms of the existing ones, e.g. convert your vertex data to 'Vertex' 'Float's,
--   turn them into 'Fragment' 'Float's with 'toFragment' and then convert them to your fragment data representation.
class GPU a => VertexOutput a where
    -- | The corresponding type in the 'FragmentStream' after rasterization.
    type FragmentInput a
    -- | Turns a vertex value into a fragment value in the 'Rasterizer' arrow. 
    toFragment :: Rasterizer a (FragmentInput a) 

instance VertexOutput (Vertex Float) where
    type  FragmentInput (Vertex Float) = Fragment Float
    toFragment = Rasterizer rasterizeVertex

instance VertexOutput () where
    type FragmentInput () = ()
    toFragment = Rasterizer id                                         
instance (VertexOutput a,VertexOutput b) => VertexOutput (a,b) where
    type FragmentInput (a,b) = (FragmentInput a, FragmentInput b)
    toFragment = proc (a, b) -> do a' <- toFragment -< a
                                   b' <- toFragment -< b
                                   returnA -< (a', b')
instance (VertexOutput a,VertexOutput b,VertexOutput c) => VertexOutput (a,b,c) where
    type FragmentInput (a,b,c) = (FragmentInput a, FragmentInput b, FragmentInput c)
    toFragment = proc (a, b, c) -> do (a', b') <- toFragment -< (a, b)
                                      c' <- toFragment -< c
                                      returnA -< (a', b', c')
instance (VertexOutput a,VertexOutput b,VertexOutput c,VertexOutput d) => VertexOutput (a,b,c,d) where
    type FragmentInput (a,b,c,d) = (FragmentInput a, FragmentInput b, FragmentInput c, FragmentInput d)
    toFragment = proc (a, b, c, d) -> do (a', b', c') <- toFragment -< (a, b, c)
                                         d' <- toFragment -< d
                                         returnA -< (a', b', c', d')

instance (VertexOutput a, VertexOutput b) => VertexOutput (a:.b) where
    type FragmentInput (a:.b) = FragmentInput a :. FragmentInput b
    toFragment = proc (a:.b) -> do a' <- toFragment -< a
                                   b' <- toFragment -< b
                                   returnA -< a':.b'

-- | Rasterize front side of all types of primitives with vertices containing canonical view coordinates into fragments.    
rasterizeFront :: VertexOutput a
          => PrimitiveStream p (VertexPosition, a) -- ^ The primitive stream with vertices containing canonical view coordinates and data to be interpolated.
          -> FragmentStream (FragmentInput a) -- ^ The resulting fragment stream with fragments containing the interpolated values.
rasterizeFront x = case x of
        (PrimitiveStreamShader xs) -> FragmentStream $ map rasterizeOne xs
        (PrimitiveStreamNoShader [] _) -> FragmentStream []
        (PrimitiveStreamNoShader xs a) -> FragmentStream [rasterizeOne (xs, a)]
    where rasterizeOne (pdesc, (pos, va)) = ((pdesc, CullBack, pos), true, getFragmentInput va)

-- | Rasterize both sides of triangles with vertices containing canonical view coordinates into fragments, also returning the primitives side in the fragments.    
rasterizeFrontAndBack :: VertexOutput a
                      => PrimitiveStream Triangle (VertexPosition, a) -- ^ The primitive stream with vertices containing canonical view coordinates and data to be interpolated.
                      -> FragmentStream (Fragment Bool, FragmentInput a) -- ^ The resulting fragment stream with fragments containing a bool saying if the primitive was front facing and the interpolated values.
rasterizeFrontAndBack x = case x of 
        (PrimitiveStreamShader xs) -> FragmentStream $ map rasterizeOne xs
        (PrimitiveStreamNoShader [] _) -> FragmentStream []
        (PrimitiveStreamNoShader xs a) -> FragmentStream [rasterizeOne (xs, a)]
    where rasterizeOne (pdesc, (pos, va)) = ((pdesc, CullNone, pos), true, (fragmentFrontFacing, getFragmentInput va))

-- | Rasterize back side of triangles with vertices containing canonical view coordinates into fragments.    
rasterizeBack :: VertexOutput a
              => PrimitiveStream Triangle (VertexPosition, a)  -- ^ The primitive stream with vertices containing canonical view coordinates and data to be interpolated.
              -> FragmentStream  (FragmentInput a) -- ^ The resulting fragment stream with fragments containing the interpolated values.
rasterizeBack x = case x of
        (PrimitiveStreamShader xs) -> FragmentStream $ map rasterizeOne xs
        (PrimitiveStreamNoShader [] _) -> FragmentStream []
        (PrimitiveStreamNoShader xs a) -> FragmentStream [rasterizeOne (xs, a)]
    where rasterizeOne (pdesc, (pos, va)) = ((pdesc, CullFront, pos), true, getFragmentInput va)

--------------------------------------
-- Private
--

getFragmentInput :: forall a. VertexOutput a => a -> FragmentInput a
getFragmentInput = fromRasterizer (toFragment :: Rasterizer a (FragmentInput a))
