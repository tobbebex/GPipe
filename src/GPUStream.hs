-----------------------------------------------------------------------------
--
-- Module      :  GPUStream
-- Copyright   :  Tobias Bexelius
-- License     :  BSD3
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- |
-----------------------------------------------------------------------------

module GPUStream (
    PrimitiveStream(..),
    FragmentStream(..),
    VertexPosition,
    CullMode(..),
    Primitive(..),
    Triangle(..),
    Line(..),
    Point(..),
    VertexSetup(..),
    PrimitiveStreamDesc,
    FragmentStreamDesc,
    filterFragments,
    loadFragmentColorStream,
    loadFragmentDepthStream,
    loadFragmentColorDepthStream,
    loadFragmentAnyStream
) where

import Shader
import Formats
import Data.Monoid
import Data.Vec (Vec4)
import Resources
import qualified Graphics.Rendering.OpenGL as GL (PrimitiveMode(..))
import Graphics.Rendering.OpenGL (cullFace, ($=), Face(..))
import Control.Arrow (first, second)

-- | A stream of primitives built by vertices on the GPU. The first parameter is the primitive type (currently 'Triangle', 'Line' or 'Point') and the second the
-- the type of each primitives' vertices' type (built up of atoms of type 'Vertex').
data PrimitiveStream p a = PrimitiveStreamNoShader PrimitiveStreamDesc a | PrimitiveStreamShader [(PrimitiveStreamDesc, a)]
-- | A stream of fragments on the GPU, parameterized on the fragments type
-- (built up of atoms of type 'Fragment').
newtype FragmentStream a = FragmentStream [(FragmentStreamDesc, Fragment Bool, a)]

type VertexPosition                               = Vec4 (Vertex Float)
data CullMode                                     = CullNone | CullFront | CullBack  deriving (Eq,Ord,Bounded,Enum,Show)
data VertexSetup                                  = VertexSetup [[Float]] | IndexedVertexSetup [[Float]] [Int]  deriving (Eq,Ord,Show)
type PrimitiveStreamDesc                          = [(GL.PrimitiveMode, VertexSetup)]
type FragmentStreamDesc                           = (PrimitiveStreamDesc, CullMode, Vec4 (Vertex Float))

instance Functor (PrimitiveStream p) where
    fmap f (PrimitiveStreamNoShader [] a)         = PrimitiveStreamNoShader [] $ f a
    fmap f (PrimitiveStreamNoShader xs a)         = PrimitiveStreamShader [(xs, f a)]
    fmap f (PrimitiveStreamShader xs)             = PrimitiveStreamShader $ map (second f) xs
instance Functor FragmentStream where
    fmap f (FragmentStream a)                     = FragmentStream $ map (\(x,y,z) -> (x, y, f z)) a

instance Monoid (PrimitiveStream p a) where
    mempty                                        = PrimitiveStreamNoShader [] undefined
    PrimitiveStreamNoShader [] _ `mappend` a      = a
    a `mappend` PrimitiveStreamNoShader [] _      = a
    PrimitiveStreamNoShader xs a `mappend` PrimitiveStreamNoShader ys _ = PrimitiveStreamNoShader (xs ++ ys) a -- Optimization!
    PrimitiveStreamShader xs `mappend` PrimitiveStreamShader ys = PrimitiveStreamShader $ xs ++ ys
    PrimitiveStreamNoShader xs a `mappend` PrimitiveStreamShader ys = PrimitiveStreamShader $ (xs, a):ys
    PrimitiveStreamShader xs `mappend` PrimitiveStreamNoShader ys a = PrimitiveStreamShader $ xs ++ [(ys, a)]
instance Monoid (FragmentStream a) where
    mempty                                        = FragmentStream []
    FragmentStream a `mappend` FragmentStream b   = FragmentStream (a ++ b)

-- | Filters out fragments in a stream where the provided function returns 'true'.
filterFragments :: (a -> Fragment Bool) -> FragmentStream a -> FragmentStream a
filterFragments f (FragmentStream xs)             = FragmentStream $ map filterOne xs
	where filterOne (fdesc, b, a)                    = (fdesc, b &&* f a, a)
	

-----------------------------------------

class Primitive p where
    getPrimitiveMode :: p -> GL.PrimitiveMode

data Triangle = TriangleStrip | TriangleList | TriangleFan deriving (Eq,Ord,Bounded,Enum,Show)
data Line = LineStrip | LineList deriving (Eq,Ord,Bounded,Enum,Show)
data Point = PointList deriving (Eq,Ord,Bounded,Enum,Show)

instance Primitive Triangle where
    getPrimitiveMode TriangleStrip = GL.TriangleStrip
    getPrimitiveMode TriangleList = GL.Triangles
    getPrimitiveMode TriangleFan = GL.TriangleFan
instance Primitive Line where
    getPrimitiveMode LineStrip = GL.LineStrip
    getPrimitiveMode LineList = GL.Lines
instance Primitive Point where
    getPrimitiveMode PointList = GL.Points

-----------------------------------------
loadFragmentColorStream :: ColorFormat f => FragmentStream (Color f (Fragment Float)) -> ContextCacheIO () -> ContextCacheIO ()
loadFragmentColorStream = loadFragmentColorStream' . fmap (fromColor 0 1)
    where loadFragmentColorStream' (FragmentStream xs) = layerMapM_ drawCallColor xs
loadFragmentDepthStream :: FragmentStream (Fragment Float) -> ContextCacheIO () -> ContextCacheIO ()
loadFragmentDepthStream (FragmentStream xs) = layerMapM_ (drawCallColorDepth  . setDefaultColor) xs
                                              where
                                                  setDefaultColor (desc, notDisc, d) = (desc, notDisc, (0,d))

loadFragmentColorDepthStream :: ColorFormat f => FragmentStream (Color f (Fragment Float), Fragment Float) -> ContextCacheIO () -> ContextCacheIO ()
loadFragmentColorDepthStream = loadFragmentColorDepthStream' . fmap (first (fromColor 0 1))
    where loadFragmentColorDepthStream' (FragmentStream xs) = layerMapM_ drawCallColorDepth xs
loadFragmentAnyStream :: FragmentStream a -> ContextCacheIO () -> ContextCacheIO ()
loadFragmentAnyStream (FragmentStream xs) = layerMapM_ (drawCallColor  . setDefaultColor) xs
                                            where
                                                setDefaultColor (desc, notDisc, _) = (desc, notDisc, 0)

layerMapM_ f (x:xs) io = layerMapM_ f xs (f x io)
layerMapM_ _ [] io = io

drawCallColor ((vss, cull, vPos), nd, c) io = drawCall vss cull io $ getShaders vPos nd c Nothing
drawCallColorDepth ((vss, cull, vPos), nd, (c,d)) io = drawCall vss cull io $ getShaders vPos nd c (Just d)

mapSelect = map . select
    where select (x:xs) ys = let (a:b) = drop x ys
                             in a: select (map (\t-> t-x-1) xs) b
          select [] _      = []

drawCall vss cull io ((vkey,vstr,vuns), (fkey,fstr,funs), ins) = do
    vss' <- mapM (evalVertexSetups ins) vss
    vkey' <- ioEvaluate vkey
    fkey' <- ioEvaluate fkey
    s <- ioEvaluate (length ins)
    vuns' <- ioEvaluate vuns
    funs' <- ioEvaluate funs
    cull' <- ioEvaluate cull
    io
    (pr, (vu, fu)) <- createProgramResource vkey' vstr fkey' fstr s
    useProgramResource pr
    useUniforms vu vuns'
    useUniforms fu funs'
    liftIO $ useCull cull'
    mapM_ (drawVertexSetups ins) vss'

evalVertexSetups ins (p, VertexSetup v) = do
    xs <- ioEvaluate (mapSelect ins v)
    vs <- ioEvaluate (length v)
    p' <- ioEvaluate p
    return (v, xs, p', vs, Nothing)
evalVertexSetups ins (p, IndexedVertexSetup v i) = do
    xs <- ioEvaluate (mapSelect ins v)
    i' <- ioEvaluate i
    vs <- ioEvaluate (length v)
    p' <- ioEvaluate p
    return (v, xs, p', vs, Just i')
   
drawVertexSetups ins (v, xs, p', vs, Nothing) = do
    vb <- createVertexBuffer xs ins v
    liftIO $ drawVertexBuffer p' vb vs
drawVertexSetups ins (v, xs, p', vs, Just i') = do
    ib <- createIndexBuffer i' vs
    vb <- createVertexBuffer xs ins v
    liftIO $ drawIndexVertexBuffer p' vb ib

useCull CullNone = cullFace $= Nothing
useCull CullFront = cullFace $= Just Front
useCull CullBack = cullFace $= Just Back
