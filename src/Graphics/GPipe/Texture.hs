-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.Texture
-- Copyright   :  Tobias Bexelius
-- License     :  BSD3
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Textures are type safe in GPipe, e.g. when you sample a @RGBFormat@ texture, you get an @RGB@ value.
--
-- Textures are either created directly from memory, or by giving a framebuffer a concrete size (which
-- it otherwise don't have). The latter is however not possible for 3D textures.
--
-- Depth textures are textures that contains depth component data (of type @DepthFormat@) but takes the type
-- of @LuminanceFormat@ or @AlphaFormat@ textures, and are sampled as such.
--
-----------------------------------------------------------------------------

module Graphics.GPipe.Texture (
    -- * Data types
    Texture3D(),
    Texture2D(),
    Texture1D(),
    TextureCube(),
    -- * Operation
    Texture(..),
    -- * Creation
    newTexture,
    newDepthTexture,
    FromFrameBufferColor(..),
    FromFrameBufferDepth(..),
    DepthColorFormat(),
    fromFrameBufferCubeColor,
    fromFrameBufferCubeDepth,
    -- * Samplers
    Sampler(..),
    Filter(..),
    EdgeMode(..),   
) where

import Resources
import Textures


