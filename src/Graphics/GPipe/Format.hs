-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.Format
-- Copyright   :  Tobias Bexelius
-- License     :  BSD3
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | This module defines the various formats that are used by 'FrameBuffer's and textures, both
-- on the GPU and the CPU.
-----------------------------------------------------------------------------

module Graphics.GPipe.Format (
    -- * GPU formats
    AlphaFormat(..),
    LuminanceFormat(..),
    LuminanceAlphaFormat(..),
    RGBFormat(..),
    RGBAFormat(..),
    DepthFormat(..),
    StencilFormat(..),
    GPUFormat(type CPUFormat),
    ColorFormat(),
    Color(..),
    Depth,
    Stencil,
    -- * CPU formats
    CPUFormat4Comp(..),
    CPUFormat3Comp(..),
    CPUFormat2Comp(..),
    CPUFormat1Comp(..),
    StorableCPUFormat(),

) where

import Formats

