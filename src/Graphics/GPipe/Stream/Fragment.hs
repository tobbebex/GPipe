-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.Stream.Fragment
-- Copyright   :  Tobias Bexelius
-- License     :  BSD3
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | 'FragmentStream's implement the 'Functor' class, which provides the
-- 'fmap' method that you can use to manipulate those streams. This corresponds to writing and using
-- fragment shaders, but in a much more modular way. You may for instance apply 'fmap'
-- several times in a sequence, effectively creating complex shaders.
--
-- Instances are also provided for the 'Monoid' class, so several streams (of the same type) can be
-- concatenated. The order is preserved, meaning that the fragments in stream @a@ in @a `mappend` b@ will be
-- drawn before the fragments in @b@.
--
-- All atomic values except textures in fragment streams uses the 'Fragment' type constructor.
-- Composite types are created by composing the atomic 'Fragment' types, rather than wrapping the
-- composite type in the 'Fragment' type constructors.
--
-- 'Fragment' instances for are provided for most of Prelude's numerical classes. Since 'Eq', 'Ord'
-- and 'Show' are prerequisites for these classes, instances are provided for them too, even though
-- their methods all will generate errors if used (except 'min' and 'max'). Use the instances of
-- 'EqB', 'OrdB' and 'IfB' from the Boolean package if you want to compare 'Fragment' values.
-- Hyperbolic trigonometrical functions aren't provided either.
--
-- Rewrite rule specializations are provided for the Vec package functions 'norm', 'normalize',
-- 'dot' and 'cross' on vectors of 'Fragment' 'Float', so the use of these functions (and others
-- from that package that is defined in terms of them) are highly encouraged.

-----------------------------------------------------------------------------

module Graphics.GPipe.Stream.Fragment (
    -- * Data types
    FragmentStream(),
    Fragment(),

    -- * Various fragment functions
    dFdx,
    dFdy,
    fwidth,
    filterFragments,

    -- * Creating fragment streams
    VertexOutput(..),
    Rasterizer(),
    VertexPosition,
    -- | When using the @rasterize@ functions, give the vertices positions in canonical view space, i.e. where @x@, @y@ and @z@
    -- is in the interval @[-1, 1]@. These aren't interpolated back to the fragments by default, so you
    -- must duplicate these positions into the vertices interpolated values if you need them in the fragments (which is very unusual).
    rasterizeFront,
    rasterizeBack,
    rasterizeFrontAndBack,
) where

import Shader
import GPUStream
import Rasterizer

