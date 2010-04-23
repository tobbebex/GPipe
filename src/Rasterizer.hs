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
import Control.Monad.Identity

-- | A monad in which vertex data gets converted to fragment data.
--   Use 'toFragment' in the existing instances of 'VertexOutput' to operate in this monad.
newtype Rasterizer a = Rasterizer {fromRasterizer :: Identity a} deriving (Functor, Monad)

-- | The context of types that can be rasterized from vertices in 'PrimitiveStream's to fragments in 'FragmentStream's.
--   Create your own instances in terms of the existing ones, e.g. convert your vertex data to 'Vertex' 'Float's,
--   turn them into 'Fragment' 'Float's with 'toFragment' and then convert them to your fragment data representation.
class GPU a => VertexOutput a where
    -- | The corresponding type in the 'FragmentStream' after rasterization.
    type FragmentInput a
    -- | Turns a vertex value into a fragment value in the 'Rasterizer' monad. 
    toFragment :: a -> Rasterizer (FragmentInput a) 

instance VertexOutput (Vertex Float) where
    type  FragmentInput (Vertex Float) = Fragment Float
    toFragment = Rasterizer . return . rasterizeVertex

instance VertexOutput () where
    type FragmentInput () = ()
    toFragment () = return ()                                         
instance (VertexOutput a,VertexOutput b) => VertexOutput (a,b) where
    type FragmentInput (a,b) = (FragmentInput a, FragmentInput b)
    toFragment (a, b) = do a' <- toFragment a
                           b' <- toFragment b
                           return (a', b')
instance (VertexOutput a,VertexOutput b,VertexOutput c) => VertexOutput (a,b,c) where
    type FragmentInput (a,b,c) = (FragmentInput a, FragmentInput b, FragmentInput c)
    toFragment (a, b, c) = do a' <- toFragment a
                              b' <- toFragment b
                              c' <- toFragment c
                              return (a', b', c')
instance (VertexOutput a,VertexOutput b,VertexOutput c,VertexOutput d) => VertexOutput (a,b,c,d) where
    type FragmentInput (a,b,c,d) = (FragmentInput a, FragmentInput b, FragmentInput c, FragmentInput d)
    toFragment (a, b, c, d) = do a' <- toFragment a
                                 b' <- toFragment b
                                 c' <- toFragment c
                                 d' <- toFragment d
                                 return (a', b', c', d')

instance (VertexOutput a, VertexOutput b) => VertexOutput (a:.b) where
    type FragmentInput (a:.b) = FragmentInput a :. FragmentInput b
    toFragment (a:.b) = do a' <- toFragment a
                           b' <- toFragment b
                           return $ a':.b'

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

getFragmentInput ::  VertexOutput a => a -> FragmentInput a
getFragmentInput = runIdentity . fromRasterizer . toFragment
