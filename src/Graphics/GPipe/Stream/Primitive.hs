-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.Stream.Primitive
-- Copyright   :  Tobias Bexelius
-- License     :  BSD3
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | 'PrimitiveStream's implement the 'Functor' class, which provides the
-- 'fmap' method that you can use to manipulate those streams. This corresponds to writing and using
-- vertex shaders, but in a much more modular way. You may for instance apply 'fmap'
-- several times in a sequence, effectively creating complex shaders.
--
-- Instances are also provided for the 'Monoid' class, so several streams (of the same type) can be
-- concatenated. The order is preserved, meaning that the primitives in stream @a@ in @a `mappend` b@ will be
-- drawn before the primitives in @b@.
--
-- All atomic values except textures in vertex streams uses the 'Vertex' type constructor.
-- Composite types are created by composing the atomic 'Vertex' types, rather than wrapping the
-- composite type in the 'Vertex' type constructors.
--
-- 'Vertex' instances for are provided for most of Prelude's numerical classes. Since 'Eq', 'Ord'
-- and 'Show' are prerequisites for these classes, instances are provided for them too, even though
-- their methods all will generate errors if used (except 'min' and 'max'). Use the instances of
-- 'EqB', 'OrdB' and 'IfB' from the Boolean package if you want to compare 'Vertex' values.
-- Hyperbolic trigonometrical functions aren't provided either.
--
-- Rewrite rule specializations are provided for the Vec package functions 'norm', 'normalize',
-- 'dot' and 'cross' on vectors of 'Vertex' 'Float', so the use of these functions (and others
-- from that package that is defined in terms of them) are highly encouraged.
-----------------------------------------------------------------------------

module Graphics.GPipe.Stream.Primitive (
    -- * Data types
    PrimitiveStream(),
    V,
    Vertex,

    -- * Creating primitive streams
    VertexInput(..),
    InputAssembler(),
    Triangle(..),
    Line(..),
    Point(..),
    Primitive(),
    toGPUStream,
    toIndexedGPUStream,  
) where


import Shader
import GPUStream
import InputAssembler






