{-# LANGUAGE UndecidableInstances, TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
--
-- Module      :  Formats
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

module Formats
(
    AlphaFormat(..),
    DepthFormat(..),
    StencilFormat(..),
    LuminanceFormat(..),
    LuminanceAlphaFormat(..),
    RGBFormat(..),
    RGBAFormat(..),
    CPUFormat4Comp(..),
    CPUFormat3Comp(..),
    CPUFormat2Comp(..),
    CPUFormat1Comp(..),
    StorableCPUFormat(toGLDataType),
    formatRowByteSize,
    GPUFormat(..),
    ColorFormat(fromColor,toColor), -- (..) will give a warning that Color is exported twice, even though we need to export it explicitly to get its constructors
    Color(..),
    Depth,
    Stencil
)
where
import qualified Graphics.Rendering.OpenGL as GL
import Data.Vec ((:.)(..), Vec3, Vec4, zipWith)
import Prelude hiding (zipWith)
import Data.Boolean
import Data.Vec.Boolean

-- | A GPU format with only an alpha value.
-- These are the associated types in 'GPUFormat' and 'ColorFormat':
--
-- [@CPUFormat AlphaFormat@] 'CPUFormat1Comp'
--
-- [@Color AlphaFormat a@] @Alpha a@
data AlphaFormat = Alpha4 | Alpha8 | Alpha12 | Alpha16 deriving (Eq,Ord,Bounded,Enum,Show)
-- | A GPU format with a single color component.
-- These are the associated types in 'GPUFormat' and 'ColorFormat':
--
-- [@CPUFormat LuminanceFormat@] 'CPUFormat1Comp'
--
-- [@Color LuminanceFormat a@] @Luminance a@
data LuminanceFormat = Luminance4 | Luminance8 | Luminance12 | Luminance16 | SLuminance8 deriving (Eq,Ord,Bounded,Enum,Show)
-- | A GPU format with a single color component and an alpha value.
-- These are the associated types in 'GPUFormat' and 'ColorFormat':
--
-- [@CPUFormat LuminanceAlphaFormat@] 'CPUFormat2Comp'
--
-- [@Color LuminanceAlphaFormat a@] @LuminanceAlpha a a@
data LuminanceAlphaFormat = Luminance4Alpha4 | Luminance6Alpha2 | Luminance8Alpha8 | Luminance12Alpha4 | Luminance12Alpha12 | Luminance16Alpha16 | SLuminance8Alpha8 deriving (Eq,Ord,Bounded,Enum,Show)
-- | A GPU format with color components for red, green and blue.
-- These are the associated types in 'GPUFormat' and 'ColorFormat':
--
-- [@CPUFormat RGBFormat@] 'CPUFormat3Comp'
--
-- [@Color RGBFormat a@] @RGB (@'Vec3'@ a)@
data RGBFormat = R3G3B2 | RGB4 | RGB5 | RGB8 | RGB10 | RGB12 | RGB16 | SRGB8 deriving (Eq,Ord,Bounded,Enum,Show)
-- | A GPU format with color components for red, green and blue, and an alpha value.
-- These are the associated types in 'GPUFormat' and 'ColorFormat':
--
-- [@CPUFormat RGBAFormat@] 'CPUFormat4Comp'
--
-- [@Color RGBAFormat a@] @RGBA (@'Vec3'@ a) a@
data RGBAFormat = RGBA2 | RGBA4 | RGB5A1 | RGBA8 | RGB10A2 | RGBA12 | RGBA16 | SRGBA8 deriving (Eq,Ord,Bounded,Enum,Show)
-- | A GPU format for a depth buffer value.
-- This is the associated type in 'GPUFormat':
--
-- [@CPUFormat DepthFormat@] 'CPUFormat1Comp'
data DepthFormat = Depth16 | Depth24 | Depth32 deriving (Eq,Ord,Bounded,Enum,Show)
-- | A GPU format for a stencil buffer value.
-- This is the associated type in 'GPUFormat':
--
-- [@CPUFormat StencilFormat@] 'CPUFormat1Comp'
data StencilFormat = StencilFormat deriving (Eq,Ord,Bounded,Enum,Show)

-- | A CPU format for 4 components (i.e. a RGBA color).
data CPUFormat4Comp = PerComp4 CPUFormat1Comp
                        | UnsignedShort4_4_4_4
                        | UnsignedShort4_4_4_4_Rev
                        | UnsignedShort5_5_5_1
                        | UnsignedShort1_5_5_5_Rev                       
                        | UnsignedInt8_8_8_8
                        | UnsignedInt8_8_8_8_Rev
                        | UnsignedInt10_10_10_2
                        | UnsignedInt2_10_10_10_Rev
                        deriving (Eq,Ord,Show)

-- | A CPU format for 3 components (i.e. a RGB color).
data CPUFormat3Comp = PerComp3 CPUFormat1Comp
                        | UnsignedByte3_3_2
                        | UnsignedByte2_3_3_Rev
                        | UnsignedShort5_6_5
                        | UnsignedShort5_6_5_Rev
                        deriving (Eq,Ord,Show)

-- | A CPU format for 2 components (i.e. a LuminanceAlpha color).
data CPUFormat2Comp = PerComp2 CPUFormat1Comp
                        deriving (Eq,Ord,Show)

-- | A CPU format for 1 component
data CPUFormat1Comp = UnsignedByteFormat
                        | BitmapFormat
                        | ByteFormat
                        | UnsignedShortFormat
                        | ShortFormat
                        | UnsignedIntFormat
                        | IntFormat
                        | FloatFormat
                        deriving (Eq,Ord,Show)

class StorableCPUFormat a where
    sizeOfFormat :: a -> Int
    toGLDataType :: a -> GL.DataType

formatRowByteSize :: StorableCPUFormat a => a -> Int -> Int
formatRowByteSize f x = (x*sizeOfFormat f-1) `div` 8 + 1

instance StorableCPUFormat CPUFormat4Comp where
    sizeOfFormat (PerComp4 a) = 4 * sizeOfFormat a 
    sizeOfFormat UnsignedShort4_4_4_4 = 16
    sizeOfFormat UnsignedShort4_4_4_4_Rev = 16
    sizeOfFormat UnsignedShort5_5_5_1 = 16
    sizeOfFormat UnsignedShort1_5_5_5_Rev = 16
    sizeOfFormat UnsignedInt8_8_8_8 = 32
    sizeOfFormat UnsignedInt8_8_8_8_Rev = 32
    sizeOfFormat UnsignedInt10_10_10_2 = 32
    sizeOfFormat UnsignedInt2_10_10_10_Rev = 32
    toGLDataType (PerComp4 a) = toGLDataType a 
    toGLDataType UnsignedShort4_4_4_4 = GL.UnsignedShort4444
    toGLDataType UnsignedShort4_4_4_4_Rev = GL.UnsignedShort4444Rev
    toGLDataType UnsignedShort5_5_5_1 = GL.UnsignedShort5551
    toGLDataType UnsignedShort1_5_5_5_Rev = GL.UnsignedShort1555Rev
    toGLDataType UnsignedInt8_8_8_8 = GL.UnsignedInt8888
    toGLDataType UnsignedInt8_8_8_8_Rev = GL.UnsignedInt8888Rev
    toGLDataType UnsignedInt10_10_10_2 = GL.UnsignedInt1010102
    toGLDataType UnsignedInt2_10_10_10_Rev = GL.UnsignedInt2101010Rev
instance StorableCPUFormat CPUFormat3Comp where
    sizeOfFormat (PerComp3 a) = 3 * sizeOfFormat a 
    sizeOfFormat UnsignedByte3_3_2 = 8
    sizeOfFormat UnsignedByte2_3_3_Rev = 8
    sizeOfFormat UnsignedShort5_6_5 = 16
    sizeOfFormat UnsignedShort5_6_5_Rev = 16
    toGLDataType (PerComp3 a) = toGLDataType a 
    toGLDataType UnsignedByte3_3_2 = GL.UnsignedByte332
    toGLDataType UnsignedByte2_3_3_Rev = GL.UnsignedByte233Rev
    toGLDataType UnsignedShort5_6_5 = GL.UnsignedShort565
    toGLDataType UnsignedShort5_6_5_Rev = GL.UnsignedShort565Rev

instance StorableCPUFormat CPUFormat2Comp where
    sizeOfFormat (PerComp2 a) = 2 * sizeOfFormat a
    toGLDataType (PerComp2 a) = toGLDataType a 

instance StorableCPUFormat CPUFormat1Comp where
    sizeOfFormat UnsignedByteFormat = 8 
    sizeOfFormat BitmapFormat = 1
    sizeOfFormat ByteFormat = 8
    sizeOfFormat UnsignedShortFormat = 16
    sizeOfFormat ShortFormat = 16
    sizeOfFormat UnsignedIntFormat = 32
    sizeOfFormat IntFormat = 32
    sizeOfFormat FloatFormat = 32
    toGLDataType UnsignedByteFormat = GL.UnsignedByte
    toGLDataType BitmapFormat = GL.Bitmap
    toGLDataType ByteFormat = GL.Byte
    toGLDataType UnsignedShortFormat = GL.UnsignedShort
    toGLDataType ShortFormat = GL.Short
    toGLDataType UnsignedIntFormat = GL.UnsignedInt
    toGLDataType IntFormat = GL.Int
    toGLDataType FloatFormat = GL.Float

class (StorableCPUFormat (CPUFormat f), Eq (CPUFormat f))=> GPUFormat f where
    type CPUFormat f
    toGLInternalFormat :: f -> GL.PixelInternalFormat
    toGLPixelFormat :: f -> GL.PixelFormat

-- | This context is used to select which types can be used in a frame buffers color buffer, and also
-- to restrict the type of a texture.
class GPUFormat f => ColorFormat f where
    data Color f :: * -> *
    fromColor :: a -> a -> Color f a -> Vec4 a
    toColor :: Vec4 a -> Color f a

type Depth = Float
type Stencil = Int

instance GPUFormat AlphaFormat where
    type CPUFormat AlphaFormat = CPUFormat1Comp
    toGLInternalFormat Alpha4 = GL.Alpha4
    toGLInternalFormat Alpha8 = GL.Alpha8
    toGLInternalFormat Alpha12 = GL.Alpha12
    toGLInternalFormat Alpha16 = GL.Alpha16
    toGLPixelFormat _ = GL.Alpha
instance GPUFormat DepthFormat where
    type CPUFormat DepthFormat = CPUFormat1Comp
    toGLInternalFormat Depth16 = GL.DepthComponent16
    toGLInternalFormat Depth24 = GL.DepthComponent24
    toGLInternalFormat Depth32 = GL.DepthComponent32
    toGLPixelFormat _ = GL.DepthComponent
instance GPUFormat StencilFormat where
    type CPUFormat StencilFormat = CPUFormat1Comp
    toGLInternalFormat = error "Stencil has no GLFormat"
    toGLPixelFormat _ = GL.StencilIndex
instance GPUFormat LuminanceFormat where
    type CPUFormat LuminanceFormat = CPUFormat1Comp
    toGLInternalFormat Luminance4 = GL.Luminance4
    toGLInternalFormat Luminance8 = GL.Luminance8
    toGLInternalFormat Luminance12 = GL.Luminance12
    toGLInternalFormat Luminance16 = GL.Luminance16
    toGLInternalFormat SLuminance8 = GL.SLuminance8
    toGLPixelFormat _ = GL.Luminance
instance GPUFormat LuminanceAlphaFormat where
    type CPUFormat LuminanceAlphaFormat = CPUFormat2Comp
    toGLInternalFormat Luminance4Alpha4 = GL.Luminance4Alpha4
    toGLInternalFormat Luminance6Alpha2 = GL.Luminance6Alpha2
    toGLInternalFormat Luminance8Alpha8 = GL.Luminance8Alpha8
    toGLInternalFormat Luminance12Alpha4 = GL.Luminance12Alpha4
    toGLInternalFormat Luminance12Alpha12 = GL.Luminance12Alpha12
    toGLInternalFormat Luminance16Alpha16 = GL.Luminance16Alpha16
    toGLInternalFormat SLuminance8Alpha8 = GL.SLuminance8Alpha8
    toGLPixelFormat _ = GL.LuminanceAlpha
instance GPUFormat RGBFormat where
    type CPUFormat RGBFormat = CPUFormat3Comp
    toGLInternalFormat R3G3B2 = GL.R3G3B2
    toGLInternalFormat RGB4 = GL.RGB4
    toGLInternalFormat RGB5 = GL.RGB5
    toGLInternalFormat RGB8 = GL.RGB8
    toGLInternalFormat RGB10 = GL.RGB10
    toGLInternalFormat RGB12 = GL.RGB12
    toGLInternalFormat RGB16 = GL.RGB16
    toGLInternalFormat SRGB8 = GL.SRGB8
    toGLPixelFormat _ = GL.RGB
instance GPUFormat RGBAFormat where
    type CPUFormat RGBAFormat = CPUFormat4Comp
    toGLInternalFormat RGBA2 = GL.RGBA2
    toGLInternalFormat RGBA4 = GL.RGBA4
    toGLInternalFormat RGB5A1 = GL.RGB5A1
    toGLInternalFormat RGBA8 = GL.RGBA8
    toGLInternalFormat RGB10A2 = GL.RGB10A2
    toGLInternalFormat RGBA12 = GL.RGBA12
    toGLInternalFormat RGBA16 = GL.RGBA16
    toGLInternalFormat SRGBA8 = GL.SRGB8Alpha8
    toGLPixelFormat _ = GL.RGBA

instance ColorFormat AlphaFormat where
    data Color AlphaFormat a = Alpha a deriving (Eq,Ord,Show)
    fromColor x _ (Alpha a) = x:.x:.x:.a:.()
    toColor (_:._:._:.d:.()) = Alpha d
instance ColorFormat LuminanceFormat where
    data Color LuminanceFormat a = Luminance a deriving (Eq,Ord,Show)
    fromColor x w (Luminance a) = a:.x:.x:.w:.()
    toColor (a:._:._:._:.()) = Luminance a
instance ColorFormat LuminanceAlphaFormat where
    data Color LuminanceAlphaFormat a = LuminanceAlpha a a deriving (Eq,Ord,Show)
    fromColor x _ (LuminanceAlpha a b) = a:.x:.x:.b:.()
    toColor (a:._:._:.d:.()) = LuminanceAlpha a d
instance ColorFormat RGBFormat where
    data Color RGBFormat a = RGB (Vec3 a) deriving (Eq,Ord,Show)
    fromColor _ w (RGB (a:.b:.c:.())) = a:.b:.c:.w:.()
    toColor (a:.b:.c:._:.()) = RGB $ a:.b:.c:.()
instance ColorFormat RGBAFormat where
    data Color RGBAFormat a = RGBA (Vec3 a) a deriving (Eq,Ord,Show)
    fromColor _ _ (RGBA (a:.b:.c:.()) d) = a:.b:.c:.d:.()
    toColor (a:.b:.c:.d:.()) = RGBA (a:.b:.c:.()) d

instance IfB bool a => IfB bool (Color AlphaFormat a) where
    ifB c (Alpha t) (Alpha e) = Alpha (ifB c t e)
instance IfB bool a => IfB bool (Color LuminanceFormat a) where
    ifB c (Luminance t) (Luminance e) = Luminance (ifB c t e)
instance IfB bool a => IfB bool (Color LuminanceAlphaFormat a) where
    ifB c (LuminanceAlpha t1 t2) (LuminanceAlpha e1 e2) = LuminanceAlpha (ifB c t1 e1) (ifB c t2 e2)
instance IfB bool a => IfB bool (Color RGBFormat a) where
    ifB c (RGB t) (RGB e) = RGB (ifB c t e)
instance IfB bool a => IfB bool (Color RGBAFormat a) where
    ifB c (RGBA t1 t2) (RGBA e1 e2) = RGBA (ifB c t1 e1) (ifB c t2 e2)
    