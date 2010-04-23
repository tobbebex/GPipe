-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.Stream
-- Copyright   :  Tobias Bexelius
-- License     :  BSD3
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | A GPipe program mainly consits of creating and manipulating streams of primitives and fragments.
-- The modules "Graphics.GPipe.Stream.Primitive" and "Graphics.GPipe.Stream.Fragment" defines those streams.
--
-- All atomic values except textures in streams uses the 'Vertex' or 'Fragment' type constructors.
-- Composite types are created by composing the atomic 'Vertex' or 'Fragment' types, rather than wrapping the
-- composite type in any of those type constructors. This module provides the common classes for those atomic types,
-- as well as reexports of imported common types and modules.
-----------------------------------------------------------------------------

module Graphics.GPipe.Stream (
    -- * Common classes
    GPU(..),
    Real'(..),
    Convert(..),
    -- * Reexports
    (:.)(..),Vec2,Vec3,Vec4,
    module Data.Vec.LinAlg,          
    module Data.Boolean
) where

import Shader
import Data.Boolean
import Data.Vec
import Data.Vec.LinAlg
import Data.Vec.Boolean()
