{-# LANGUAGE RankNTypes, TypeOperators, FlexibleInstances, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  InputAssembler
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

module InputAssembler (
    InputAssembler(),
    VertexInput(..),
    toGPUStream,
    toIndexedGPUStream,
) where

import Control.Monad.Trans.State.Lazy
import GPUStream
import Shader
import Data.Vec ((:.)(..), Vec2, Vec3, Vec4)

-- | A monad in which CPU data gets converted to vertex data.
--   Use 'toVertex' in the existing instances of 'VertexInput' to operate in this monad.
newtype InputAssembler a = InputAssembler {fromInputAssembler :: State [Float] a} deriving Monad

-- | The context of types that can be converted into vertices in 'PrimitiveStream's.
--   Create your own instances in terms of the existing ones, e.g. convert your vertex data to 'Float's,
--   turn them into 'Vertex' 'Float's with 'toVertex' and then convert them to your vertex data representation.
class GPU a => VertexInput a where
    -- | Turns an ordinary value into a vertex value in the 'InputAssembler' monad. The following rule must be satisfied:
    --   
    -- > toVertex undefined >> a   =  a
    --
    --   This ensures that its definition always use the same series of 'toVertex' calls to convert values of the same type.
    --   This unfortunatly rules out ordinary lists (but instances for fixed length lists from the Vec package are however provided). 
    toVertex :: CPU a -> InputAssembler a

instance VertexInput (Vertex Float) where
    toVertex a = InputAssembler $ do x <- gets length
                                     modify (a:)
                                     return $ inputVertex x
instance VertexInput () where
    toVertex ~() = return ()                                         
instance (VertexInput a,VertexInput b) => VertexInput (a,b) where
    toVertex ~(a, b) = do a' <- toVertex a
                          b' <- toVertex b
                          return (a', b')
instance (VertexInput a,VertexInput b,VertexInput c) => VertexInput (a,b,c) where
    toVertex ~(a, b, c) = do a' <- toVertex a
                             b' <- toVertex b
                             c' <- toVertex c
                             return (a', b', c')
instance (VertexInput a,VertexInput b,VertexInput c,VertexInput d) => VertexInput (a,b,c,d) where
    toVertex ~(a, b, c, d) = do a' <- toVertex a
                                b' <- toVertex b
                                c' <- toVertex c
                                d' <- toVertex d
                                return (a', b', c', d')

instance (VertexInput a, VertexInput b) => VertexInput (a:.b) where
    toVertex ~(a:.b) = do a' <- toVertex a
                          b' <- toVertex b
                          return $ a':.b'
                                                  
-- | Converts a list of values to a 'PrimitiveStream', using a specified 'Primitive' type.
-- This function is lazy in the aspect that if parts of the values aren't used on the GPU, they won't
-- get evaluated and transferred there either.  
toGPUStream :: (VertexInput a, Primitive p)
            => p -- ^ The primitive type.
            -> [CPU a] -- ^ A list of vertices, with the layout specified by the primitive type.
            -> PrimitiveStream p a -- ^ The resulting 'PrimitiveStream'.
toGPUStream _ [] = PrimitiveStreamNoShader [] undefined
toGPUStream p xs = let (a, fs) = getVertexInput xs
                   in PrimitiveStreamNoShader [(getPrimitiveMode p, VertexSetup fs)] a

-- | Converts a list of values to a 'PrimitiveStream', using a specified 'Primitive' type and an index list.
-- This will use index buffer objects on the GPU, and is recommended if several primitives share vertices.
-- This function is lazy in the aspect that if parts of the values aren't used on the GPU, they won't
-- get evaluated and transferred there either.  
toIndexedGPUStream :: (VertexInput a, Primitive p)
                   => p  -- ^ The primitive type.
                   -> [CPU a] -- ^ A list of vertices.
                   -> [Int] -- ^ A list of indexes into the vertex list, with the layout specified by the primitive type.
                   -> PrimitiveStream p a -- ^ The resulting 'PrimitiveStream'.
toIndexedGPUStream _ [] _ = PrimitiveStreamNoShader [] undefined
toIndexedGPUStream p xs i = let (a, fs) = getVertexInput xs
                            in PrimitiveStreamNoShader [(getPrimitiveMode p, IndexedVertexSetup fs i)] a


--------------------------------------
-- Private
--

getVertexInput :: forall a. VertexInput a => [CPU a] -> (a, [[Float]])
getVertexInput xs = let readInput :: CPU a -> (a, [Float])
                        readInput = flip runState [] . fromInputAssembler . toVertex
                        e = "The method toVertex of an instance of Graphics.GPipe.Stream.Primitive.VertexInput is strict in it's input. Remember that 'toVertex undefined >> a' must be equal to 'a'. Contact the GPipe author for more information."
                        in (fst $ readInput (error e :: CPU a), map (reverse . snd  . readInput) xs)
