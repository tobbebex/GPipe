-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.FrameBuffer
-- Copyright   :  Tobias Bexelius
-- License     :  BSD3
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | 'FrameBuffer's are 2D images in which fragments from 'FragmentStream's are painted. A 'FrameBuffer'
-- may contain any combination of a color buffer, a depth buffer and a stencil buffer.
-- 'FrameBuffer's may be shown in windows, saved to memory or converted to textures.
-- 'FrameBuffer's have no size, but takes the size of the window when shown, or are given a size when
-- saved to memory or converted to a texture.
-----------------------------------------------------------------------------

module Graphics.GPipe.FrameBuffer (
    -- * The data type
    FrameBuffer(),
    -- * Displaying framebuffers
    newWindow,
    -- * Creation
    -- | These functions create new 'FrameBuffer's with initial color, depth values and\/or stencil values.
    newFrameBufferColor,
    newFrameBufferColorDepth,
    newFrameBufferColorStencil,
    newFrameBufferColorDepthStencil,
    newFrameBufferDepth,
    newFrameBufferStencil,
    newFrameBufferDepthStencil,
    -- * Data retrieval
    -- | These functions provides the means for saving a 'FrameBuffer' to main memory without the need to
    -- show it in a window.
    getFrameBufferColor,
    getFrameBufferDepth,
    getFrameBufferStencil,
    getFrameBufferCPUFormatByteSize,
    -- * Paint operations
    -- | These functions paint 'FragmentStream's on 'FrameBuffer's. A lot of different functions are
    -- provided for different types of 'FrameBuffer's and 'FragmentStream's, all which takes more or less
    -- state values. The preffered way of using those is to curry them into the specific  functions you need
    -- in your GPipe program, e.g.
    --
    -- @paintSolid =@ 'paintColorRastDepth' 'Lequal' 'True' 'NoBlending' @(RGB (vec@ 'True'@))@
    --
    -- The @RastDepth@-functions uses the rasterized depth for the fragments.
    --
    -- Functions with two 'StencilOps' arguments use them in this order: First if stencil test fail, second if stencil test pass.
    -- Functions with three 'StencilOps' arguments use them in this order: First if stencil test fail, second if depth test fail, third if depth test pass.
    paintColor,
    paintDepth,
    paintColorDepth,
    paintStencil,
    paintDepthStencil,
    paintColorStencil,
    paintColorDepthStencil,
    paintRastDepth,
    paintColorRastDepth,
    paintRastDepthStencil,
    paintColorRastDepthStencil,
    ColorMask, Blending(..), BlendEquation(..), BlendingFactor(..), LogicOp(..),
    ComparisonFunction(..), DepthFunction, DepthMask,
    StencilOps(..), StencilOp(..), StencilTest(..), StencilTests(..),
    FragmentDepth,
) where

import OutputMerger




