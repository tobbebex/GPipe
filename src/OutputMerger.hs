{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  OutputMerger
-- Copyright   :  Tobias Bexelius
-- License     :  BSD3
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- |

-----------------------------------------------------------------------------

module OutputMerger (
    FragmentDepth,
    ColorMask, Blending(..), BlendEquation(..), BlendingFactor(..), LogicOp(..),
    ComparisonFunction(..), DepthFunction, DepthMask,
    StencilOps(..), StencilOp(..), StencilTest(..), StencilTests(..),
    FrameBuffer(),
    newFrameBufferColor,
    newFrameBufferColorDepth,
    newFrameBufferColorDepthStencil,
    newFrameBufferColorStencil,
    newFrameBufferDepth,
    newFrameBufferDepthStencil,
    newFrameBufferStencil,
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
    getFrameBufferCPUFormatByteSize,
    getFrameBufferColor,
    getFrameBufferDepth,
    getFrameBufferStencil,
    newWindow,
    runFrameBufferInContext
) where

import Formats
import Shader
import GPUStream
import Resources
import Graphics.Rendering.OpenGL hiding (RGBA, Blend, stencilMask, Color, ColorBuffer, DepthBuffer, StencilBuffer, Vertex)
import qualified Graphics.Rendering.OpenGL as GL
import Data.Vec (vec, (:.)(..), Vec2)
import qualified Graphics.UI.GLUT as GLUT
import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Graphics.UI.GLUT
    (reshapeCallback, displayCallback, Window)
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Control.Exception (evaluate)
import Control.Monad.Trans.Reader (runReaderT)

type FragmentDepth = Fragment Float
-- | 'True' for each color component that should be written to the 'FrameBuffer'.
type ColorMask f = Color f Bool
-- | 'True' if the depth component should be written to the 'FrameBuffer'.
type DepthMask = Bool
-- | The function used to compare the fragment's depth and the depth buffers depth with.
type DepthFunction = ComparisonFunction

-- | Sets how the painted colors are blended with the 'FrameBuffer's previous value.
data Blending = NoBlending -- ^ The painted fragment completely overwrites the previous value.
              | Blend (BlendEquation, BlendEquation)
                      ((BlendingFactor, BlendingFactor), (BlendingFactor, BlendingFactor))
                      (Color RGBAFormat Float) -- ^ Use blending equations to combine the fragment with the previous value.
                                               -- The first 'BlendEquation' and 'BlendingFactor's is used for front faced triangles and other primitives,
                                               -- and the second for back faced triangles.
              | BlendLogicOp LogicOp -- ^ Use a 'LogicOp' to combine the fragment with the previous value.
              deriving (Eq,Ord,Show)

-- | Sets the operations that should be performed on the 'FrameBuffer's stencil value
data StencilOps = StencilOps {
                       frontStencilOp :: StencilOp, -- ^ Used for front faced triangles and other primitives.
                       backStencilOp :: StencilOp -- ^ Used for back faced triangles.
                  }  deriving (Eq,Ord,Show)

-- | Sets the tests that should be performed on the stencil value, first for front facing triangles and other primitives, then for back facing triangles.
data StencilTests = StencilTests StencilTest StencilTest  deriving (Eq,Ord,Show)
-- | Sets a test that should be performed on the stencil value.
data StencilTest = StencilTest {
                       stencilComparision :: ComparisonFunction, -- ^ The function used to compare the @stencilReference@ and the stencil buffers value with.
                       stencilReference :: Int32, -- ^ The value to compare with the stencil buffer's value. 
                       stencilMask :: Word32 -- ^ A bit mask with ones in each position that should be compared and written to the stencil buffer.
                   } deriving (Eq,Ord,Show)

------------------------------------------------------------------

-- | A polymorphic frame buffer. It is parameterized on the type of color buffer, depth buffer and stencil buffer.
-- Any instances of 'ColorFormat' can be used for color buffer, or '()' to denote "no color buffer".
-- For depth and stencil buffers, 'DepthFormat' and 'StencilFormat' marks the existance of buffer, while '()'
-- marks the inexistance.  
data FrameBuffer c d s = FrameBuffer (ContextCacheIO ())

newFrameBufferColor :: ColorFormat f =>  Color f Float -> FrameBuffer f () ()
newFrameBufferColorDepth :: ColorFormat f => Color f Float -> Depth -> FrameBuffer f DepthFormat ()
newFrameBufferColorDepthStencil :: ColorFormat f => Color f Float -> Depth -> Stencil -> FrameBuffer f DepthFormat StencilFormat
newFrameBufferColorStencil :: ColorFormat f => Color f Float -> Stencil -> FrameBuffer f () StencilFormat
newFrameBufferDepth :: Depth -> FrameBuffer () DepthFormat ()
newFrameBufferDepthStencil :: Depth -> Stencil -> FrameBuffer () DepthFormat StencilFormat
newFrameBufferStencil :: Stencil -> FrameBuffer () () StencilFormat

ioEvaluateColor z e x = let (a:.b:.c:.d:.()) = fromColor z e x
                        in do a' <- ioEvaluate a
                              b' <- ioEvaluate b
                              c' <- ioEvaluate c
                              d' <- ioEvaluate d
                              return (a':.b':.c':.d':.())

setDefaultStates :: IO ()
setDefaultStates = do frontFace $= CCW
                      depthRange $= (0,1)
                       

newFrameBufferColor c = FrameBuffer $ do
    c' <- ioEvaluateColor 0 1 c
    setContextWindow
    liftIO $ do setDefaultStates
                setNewColor c'
                clear [GL.ColorBuffer]
newFrameBufferColorDepth c d = FrameBuffer $ do
    c' <- ioEvaluateColor 0 1 c
    d' <- ioEvaluate d
    setContextWindow
    liftIO $ do setDefaultStates
                setNewColor c'
                setNewDepth d'
                clear [GL.ColorBuffer, GL.DepthBuffer]
newFrameBufferColorDepthStencil c d s = FrameBuffer $ do
    c' <- ioEvaluateColor 0 1 c
    d' <- ioEvaluate d
    s' <- ioEvaluate s
    setContextWindow
    liftIO $ do setDefaultStates
                setNewColor c'
                setNewDepth d'
                setNewStencil s'
                clear [GL.ColorBuffer, GL.DepthBuffer, GL.StencilBuffer]

newFrameBufferColorStencil c s = FrameBuffer $ do
    c' <- ioEvaluateColor 0 1 c
    s' <- ioEvaluate s
    setContextWindow
    liftIO $ do setDefaultStates
                setNewColor c'
                setNewStencil s'
                clear [GL.ColorBuffer, GL.StencilBuffer]
newFrameBufferDepth d = FrameBuffer $ do
    d' <- ioEvaluate d
    setContextWindow
    liftIO $ do setDefaultStates
                setNewDepth d'
                clear [GL.DepthBuffer]
newFrameBufferDepthStencil d s = FrameBuffer $ do
    d' <- ioEvaluate d
    s' <- ioEvaluate s
    setContextWindow
    liftIO $ do setDefaultStates
                setNewDepth d'
                setNewStencil s'
                clear [GL.DepthBuffer, GL.StencilBuffer]

newFrameBufferStencil s = FrameBuffer $ do
    s' <- ioEvaluate s
    setContextWindow
    liftIO $ do setDefaultStates
                setNewStencil s'
                clear [GL.StencilBuffer]


setNewColor (x:.y:.z:.w:.()) = clearColor $= Color4 (realToFrac x) (realToFrac y) (realToFrac z) (realToFrac w)
setNewDepth d = clearDepth $= realToFrac d
setNewStencil s = clearStencil $= fromIntegral s

------------------------------------------------------------------

paintColor :: ColorFormat c => Blending -> ColorMask c -> FragmentStream (Color c (Fragment Float)) -> FrameBuffer c d s -> FrameBuffer c d s
paintDepth :: DepthFunction -> DepthMask -> FragmentStream FragmentDepth -> FrameBuffer c DepthFormat s -> FrameBuffer c DepthFormat s 
paintColorDepth :: ColorFormat c => DepthFunction -> DepthMask -> Blending -> ColorMask c -> FragmentStream (Color c (Fragment Float), FragmentDepth) -> FrameBuffer c DepthFormat s -> FrameBuffer c DepthFormat s 
paintStencil :: StencilTests -> StencilOps -> StencilOps -> FragmentStream (Fragment a) -> FrameBuffer c d StencilFormat -> FrameBuffer c d StencilFormat
paintDepthStencil :: StencilTests -> StencilOps -> DepthFunction -> DepthMask -> StencilOps -> StencilOps -> FragmentStream FragmentDepth -> FrameBuffer c DepthFormat StencilFormat -> FrameBuffer c DepthFormat StencilFormat
paintColorStencil :: ColorFormat c => StencilTests -> StencilOps -> StencilOps -> Blending -> ColorMask c -> FragmentStream (Color c (Fragment Float)) -> FrameBuffer c d StencilFormat -> FrameBuffer c d StencilFormat
paintColorDepthStencil :: ColorFormat c => StencilTests -> StencilOps -> DepthFunction -> DepthMask -> StencilOps -> StencilOps -> Blending -> ColorMask c -> FragmentStream (Color c (Fragment Float), FragmentDepth) -> FrameBuffer c DepthFormat StencilFormat -> FrameBuffer c DepthFormat StencilFormat

paintRastDepth :: DepthFunction -> DepthMask -> FragmentStream (Fragment a) -> FrameBuffer c DepthFormat s -> FrameBuffer c DepthFormat s 
paintColorRastDepth :: ColorFormat c => DepthFunction -> DepthMask -> Blending -> ColorMask c -> FragmentStream (Color c (Fragment Float)) -> FrameBuffer c DepthFormat s -> FrameBuffer c DepthFormat s 
paintRastDepthStencil :: StencilTests -> StencilOps -> DepthFunction -> DepthMask -> StencilOps -> StencilOps -> FragmentStream (Fragment a) -> FrameBuffer c DepthFormat StencilFormat -> FrameBuffer c DepthFormat StencilFormat
paintColorRastDepthStencil :: ColorFormat c => StencilTests -> StencilOps -> DepthFunction -> DepthMask -> StencilOps -> StencilOps -> Blending -> ColorMask c -> FragmentStream (Color c (Fragment Float)) -> FrameBuffer c DepthFormat StencilFormat -> FrameBuffer c DepthFormat StencilFormat

------------------------------------------------------------------

paintColor _ _ (FragmentStream []) fb = fb
paintColor b c s (FrameBuffer io) = FrameBuffer $ loadFragmentColorStream s $ do
                                                     b'<-ioEvaluate b
                                                     c'<-ioEvaluateColor False False c
                                                     io
                                                     liftIO $ do
                                                         loadBlending b'
                                                         loadColorMask c'
                                                         depthFunc $= Nothing
                                                         stencilTest $= Disabled

paintDepth _ _ (FragmentStream []) fb = fb
paintDepth f d s (FrameBuffer io) = FrameBuffer $ loadFragmentDepthStream s $ do
                                                      f'<-ioEvaluate f
                                                      d'<-ioEvaluate d
                                                      io
                                                      liftIO $ do
                                                          depthFunc $= Just f'
                                                          depthMask $= toCapability d'
                                                          loadColorMask (vec False)
                                                          stencilTest $= Disabled

paintColorDepth _ _ _ _ (FragmentStream []) fb = fb
paintColorDepth f d b c s (FrameBuffer io) = FrameBuffer $ loadFragmentColorDepthStream s $ do
                                                              b'<-ioEvaluate b
                                                              c'<-ioEvaluateColor False False c
                                                              f'<-ioEvaluate f
                                                              d'<-ioEvaluate d
                                                              io
                                                              liftIO $ do
                                                                  loadBlending b'
                                                                  loadColorMask c'
                                                                  depthFunc $= Just f'
                                                                  depthMask $= toCapability d'
                                                                  stencilTest $= Disabled

paintStencil _ _ _ (FragmentStream []) fb = fb
paintStencil t sf p s (FrameBuffer io) = FrameBuffer $ loadFragmentAnyStream s $ do
                                                          t'<-ioEvaluate t
                                                          sf'<-ioEvaluate sf
                                                          p'<-ioEvaluate p
                                                          io
                                                          liftIO $ do
                                                              loadStencilTests t'
                                                              loadStencilOps sf' sf' p'
                                                              depthFunc $= Nothing
                                                              loadColorMask (vec False)

paintDepthStencil _ _ _ _ _ _ (FragmentStream []) fb = fb
paintDepthStencil t sf f d df p s (FrameBuffer io) = FrameBuffer $ loadFragmentDepthStream s $ do
                                                                      t'<-ioEvaluate t
                                                                      sf'<-ioEvaluate sf
                                                                      f'<-ioEvaluate f
                                                                      d'<-ioEvaluate d
                                                                      df'<-ioEvaluate df
                                                                      p'<-ioEvaluate p
                                                                      io
                                                                      liftIO $ do
                                                                          loadStencilTests t'
                                                                          loadStencilOps sf' df' p'
                                                                          depthFunc $= Just f'
                                                                          depthMask $= toCapability d'
                                                                          loadColorMask (vec False)

paintColorStencil _ _ _ _ _ (FragmentStream []) fb = fb
paintColorStencil t sf p b c s (FrameBuffer io) = FrameBuffer $ loadFragmentColorStream s $ do
                                                                   t'<-ioEvaluate t
                                                                   sf'<-ioEvaluate sf
                                                                   p'<-ioEvaluate p
                                                                   b'<-ioEvaluate b
                                                                   c'<-ioEvaluateColor False False c
                                                                   io
                                                                   liftIO $ do
                                                                       loadStencilTests t'
                                                                       loadStencilOps sf' sf' p'
                                                                       depthFunc $= Nothing
                                                                       loadBlending b'
                                                                       loadColorMask c'

paintColorDepthStencil _ _ _ _ _ _ _ _ (FragmentStream []) fb = fb
paintColorDepthStencil t sf f d df p b c s (FrameBuffer io) = FrameBuffer $ loadFragmentColorDepthStream s $ do
                                                                               t'<-ioEvaluate t
                                                                               sf'<-ioEvaluate sf
                                                                               f'<-ioEvaluate f
                                                                               d'<-ioEvaluate d
                                                                               df'<-ioEvaluate df
                                                                               p'<-ioEvaluate p
                                                                               b'<-ioEvaluate b
                                                                               c'<-ioEvaluateColor False False c
                                                                               io
                                                                               liftIO $ do
                                                                                   loadStencilTests t'
                                                                                   loadStencilOps sf' df' p'
                                                                                   depthFunc $= Just f'
                                                                                   depthMask $= toCapability d'
                                                                                   loadBlending b'
                                                                                   loadColorMask c'

paintRastDepth _ _ (FragmentStream []) fb = fb
paintRastDepth f d s (FrameBuffer io) = FrameBuffer $ loadFragmentAnyStream s $ do
                                                        f'<-ioEvaluate f
                                                        d'<-ioEvaluate d
                                                        io
                                                        liftIO $ do
                                                            depthFunc $= Just f'
                                                            depthMask $= toCapability d'
                                                            loadColorMask (vec False)
                                                            stencilTest $= Disabled

paintColorRastDepth _ _ _ _ (FragmentStream []) fb = fb
paintColorRastDepth f d b c s (FrameBuffer io) = FrameBuffer $ loadFragmentColorStream s $ do
                                                                 f'<-ioEvaluate f
                                                                 d'<-ioEvaluate d
                                                                 b'<-ioEvaluate b
                                                                 c'<-ioEvaluateColor False False c
                                                                 io
                                                                 liftIO $ do
                                                                     loadBlending b'
                                                                     loadColorMask c'
                                                                     depthFunc $= Just f'
                                                                     depthMask $= toCapability d'
                                                                     stencilTest $= Disabled

paintRastDepthStencil _ _ _ _ _ _ (FragmentStream []) fb = fb
paintRastDepthStencil t sf f d df p s (FrameBuffer io) = FrameBuffer $ loadFragmentAnyStream s $ do
                                                                         t'<-ioEvaluate t
                                                                         sf'<-ioEvaluate sf
                                                                         f'<-ioEvaluate f
                                                                         d'<-ioEvaluate d
                                                                         df'<-ioEvaluate df
                                                                         p'<-ioEvaluate p
                                                                         io
                                                                         liftIO $ do
                                                                             loadStencilTests t'
                                                                             loadStencilOps sf' df' p'
                                                                             depthFunc $= Just f'
                                                                             depthMask $= toCapability d'
                                                                             loadColorMask (vec False)

paintColorRastDepthStencil _ _ _ _ _ _ _ _ (FragmentStream []) fb = fb
paintColorRastDepthStencil t sf f d df p b c s (FrameBuffer io) = FrameBuffer $ loadFragmentColorStream s $ do
                                                                                  t'<-ioEvaluate t
                                                                                  sf'<-ioEvaluate sf
                                                                                  f'<-ioEvaluate f
                                                                                  d'<-ioEvaluate d
                                                                                  df'<-ioEvaluate df
                                                                                  p'<-ioEvaluate p
                                                                                  b'<-ioEvaluate b
                                                                                  c'<-ioEvaluateColor False False c
                                                                                  io
                                                                                  liftIO $ do
                                                                                      loadStencilTests t'
                                                                                      loadStencilOps sf' df' p'
                                                                                      depthFunc $= Just f'
                                                                                      depthMask $= toCapability d'
                                                                                      loadBlending b'
                                                                                      loadColorMask c'

--------------------------------------
-- | Returns the byte size needed to store a certain format and size of a framebuffer. Use this to
-- allocate memory before using 'getFrameBufferColor', 'getFrameBufferDepth' or 'getFrameBufferStencil'.
getFrameBufferCPUFormatByteSize :: StorableCPUFormat f
                                => f    -- ^ The format to store data to
                                -> Vec2 Int -- ^ The size to give the frame buffer
                                -> Int -- ^ The size in bytes of the data
getFrameBufferCPUFormatByteSize f (w:.h:.()) = h*formatRowByteSize f w

-- | Saves a 'FrameBuffer's color buffer to main memory.
getFrameBufferColor :: forall c d s a. GPUFormat c
                    => CPUFormat c    -- ^ The format to store data to
                    -> Vec2 Int    -- ^ The size to give the frame buffer
                    -> FrameBuffer c d s  -- ^ A frame buffer with a color buffer
                    -> Ptr a -- ^ A pointer to the memory where the data will be saved
                    -> IO ()
getFrameBufferColor f s@(w:.h:.()) fb p = do
    cache <- getCurrentOrSetHiddenContext
    runFrameBufferInContext cache s fb
    readPixels (Position 0 0) (Size (fromIntegral w) (fromIntegral h)) (PixelData (toGLPixelFormat (undefined :: c)) (toGLDataType f) p)

-- | Saves a 'FrameBuffer's depth buffer to main memory.
getFrameBufferDepth :: CPUFormat DepthFormat -- ^ The format to store data to
                    -> Vec2 Int -- ^ The size to give the frame buffer
                    -> FrameBuffer c DepthFormat s -- ^ A frame buffer with a depth buffer
                    -> Ptr a -- ^ A pointer to the memory where the data will be saved
                    -> IO ()
getFrameBufferDepth f s@(w:.h:.()) fb p = do
    cache <- getCurrentOrSetHiddenContext
    runFrameBufferInContext cache s fb
    readPixels (Position 0 0) (Size (fromIntegral w) (fromIntegral h)) (PixelData DepthComponent (toGLDataType f) p)

-- | Saves a 'FrameBuffer's stencil buffer to main memory.
getFrameBufferStencil :: CPUFormat StencilFormat -- ^ The format to store data to
                      -> Vec2 Int -- ^ The size to give the frame buffer
                      -> FrameBuffer c d StencilFormat -- ^ A frame buffer with a stencil buffer
                      -> Ptr a -- ^ A pointer to the memory where the data will be saved
                      -> IO ()
getFrameBufferStencil f s@(w:.h:.()) fb p = do
    cache <- getCurrentOrSetHiddenContext
    runFrameBufferInContext cache s fb
    readPixels (Position 0 0) (Size (fromIntegral w) (fromIntegral h)) (PixelData StencilIndex (toGLDataType f) p)

-- | Creates and shows a new GPipe window. Use the last parameter to add extra GLUT callbacks to the window. Note that you can't register your own 'displayCallback' and 'reshapeCallback'.
newWindow :: String     -- ^ The window title
          -> Vec2 Int   -- ^ The window position
          -> Vec2 Int   -- ^ The window size
          -> (Vec2 Int -> IO (FrameBuffer c d s)) -- ^ This function is evaluated every time the window needs to be redrawn, and the resulting 'FrameBuffer' will be drawn in the window. The parameter is the current size of the window.
          -> (Window -> IO ()) -- ^ Extra optional initialization of the window. The provided 'Window' should not be used outside this function.
          -> IO ()
newWindow name (x:.y:.()) (w:.h:.()) f xio =
    do GLUT.initialWindowPosition $= Position (fromIntegral x) (fromIntegral y)
       GLUT.initialWindowSize $= Size (fromIntegral w) (fromIntegral h)
       GLUT.initialDisplayMode $= [ GLUT.DoubleBuffered, GLUT.RGBMode, GLUT.WithAlphaComponent, GLUT.WithDepthBuffer, GLUT.WithStencilBuffer] --Enable everything, it might be needed for textures 
       w <- GLUT.createWindow name
       xio w
       newContextCache w
       displayCallback $= do cache <- liftM fromJust $ getContextCache w --We need to do this to get the correct size
                             let Size x y = contextViewPort cache
                             FrameBuffer io <- f (fromIntegral x :. fromIntegral y :. ())
                             runReaderT io cache
                             GLUT.swapBuffers
       reshapeCallback $= Just (changeContextSize w)


runFrameBufferInContext :: ContextCache -> Vec2 Int -> FrameBuffer c d s -> IO ()
runFrameBufferInContext c (a:.b:.()) (FrameBuffer io) = do
    a' <- evaluate a
    b' <- evaluate b
    runReaderT io $ c {contextViewPort = Size (fromIntegral a') (fromIntegral b')}
    finish
--------------------------------------
-- Private
--
toCapability True = Enabled
toCapability False = Disabled

loadColorMask (r:.g:.b:.a:.()) = colorMask $= Color4 (toCapability r) (toCapability g) (toCapability b) (toCapability a)

loadBlending NoBlending            = do blend $= Disabled
                                        logicOp $= Nothing
loadBlending (Blend e f (RGBA (r:.g:.b:.()) a)) = do
                                           blend $= Enabled
                                           logicOp $= Nothing
                                           blendColor $= Color4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
                                           blendEquationSeparate $= e
                                           blendFuncSeparate $= f
loadBlending (BlendLogicOp op)     =    logicOp $= Just op

loadStencilTests (StencilTests f b) = do stencilTest $= Enabled
                                         stencilFuncSeparate Front $= (stencilComparision f, fromIntegral $ stencilReference f, fromIntegral $ stencilMask f)
                                         stencilFuncSeparate Back $= (stencilComparision b, fromIntegral $ stencilReference b, fromIntegral $ stencilMask b)

loadStencilOps sf df p = do stencilOpSeparate Front $= (frontStencilOp sf, frontStencilOp df, frontStencilOp p)
                            stencilOpSeparate Back $= (backStencilOp sf, backStencilOp df, backStencilOp p)


