-----------------------------------------------------------------------------
--
-- Module      :  Resources
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

module Resources (
    createProgramResource,
    createVertexBuffer,
    createIndexBuffer,
    useUniforms,
    useProgramResource,
    drawIndexVertexBuffer,
    drawVertexBuffer,
    VertexBuffer(..),
    IndexBuffer(..),
    UniformLocationSet,
    UniformSet,
    ContextCacheIO,
    ContextCache(contextWindow,contextViewPort),
    newContextCache,
    setContextWindow,
    liftIO,
    hiddenWindowContextCache,
    WinMappedTexture,
    newWinMappedTexture,
    bindWinMappedTexture,
    Sampler(..),
    Filter(..),
    EdgeMode(..),
    SamplerType(..),
    cubeMapTargets,
    getContextCache,
    saveContextCache,
    changeContextSize,
    getCurrentOrSetHiddenContext,
    ioEvaluate,
    evaluateDeep,
    evaluatePtr,
    ShaderInfo,
    ShaderKey,
    ShaderKeyNode(..),
    Op,
    Const(..),
    Uniform(..)
) where

import Graphics.Rendering.OpenGL hiding (Sampler3D, Sampler2D, Sampler1D, SamplerCube, Point, Linear, Clamp, Uniform)
import qualified Data.HashTable as HT
import qualified Graphics.UI.GLUT as GLUT
import Data.Map (Map)
import qualified Data.Map as Map
import System.Mem.StableName
import Data.Bits
import Control.Monad.Reader
import Control.Monad
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal
import Data.List
import Data.Maybe
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Control.Exception (evaluate)
import Foreign.ForeignPtr
import System.Mem.Weak (addFinalizer)
import Data.Unique
import Data.ListTrie.Patricia.Map (TrieMap)
import qualified Data.ListTrie.Patricia.Map as TrieMap
import Data.ListTrie.Base.Map (WrappedIntMap)

data VertexBuffer = VertexBuffer BufferObject Int
data IndexBuffer = IndexBuffer BufferObject Int DataType
type UniformLocationSet = (UniformLocation,UniformLocation,UniformLocation, Map SamplerType UniformLocation)

data SamplerType = Sampler3D | Sampler2D | Sampler1D | SamplerCube deriving (Eq, Ord, Enum, Bounded)
type UniformSet = ([Float],[Int],[Bool],Map SamplerType [(Sampler, WinMappedTexture)])

type ShaderInfo = (ShaderKey, String, UniformSet)
type ShaderKey = ([Int], [(ShaderKeyNode, [Int])])
data ShaderKeyNode = ShaderKeyUniform 
                   | ShaderKeyConstant !Const
                   | ShaderKeyInput !Int
                   | ShaderKeyOp !Op
                   deriving (Eq, Ord)
type Op = String
data Const = ConstFloat Float
           | ConstInt Int
           | ConstBool Bool
           deriving (Eq, Ord)
data Uniform = UniformFloat Float
             | UniformInt Int
             | UniformBool Bool
             | UniformSampler SamplerType Sampler WinMappedTexture
                    
-- | A structure describing how a texture is sampled
data Sampler = Sampler Filter EdgeMode deriving (Eq, Ord)
-- | Filter mode used in sampler state
data Filter = Point | Linear
            deriving (Eq,Ord,Bounded,Enum,Show)
-- | Edge mode used in sampler state
data EdgeMode = Wrap | Mirror | Clamp
              deriving (Eq,Ord,Bounded,Enum,Show)

toGLFilter Point = ((Nearest, Just Nearest), Nearest)
toGLFilter Linear = ((Linear', Just Linear'), Linear')
toGLWrap Wrap = (Repeated, Repeat)
toGLWrap Mirror = (Mirrored, Repeat)
toGLWrap Clamp = (Repeated, ClampToEdge)

type ProgramCacheValue = (Program, (UniformLocationSet,UniformLocationSet))
type ProgramCacheMap = TrieMap Map (ShaderKeyNode, [Int]) (TrieMap Map (ShaderKeyNode, [Int]) (TrieMap WrappedIntMap Int (TrieMap WrappedIntMap Int ProgramCacheValue)))
type ProgramCache = IORef ProgramCacheMap -- (Map (ShaderKey, ShaderKey) (Program, (UniformLocationSet,UniformLocationSet)))
pCacheLookup :: ShaderKey -> ShaderKey -> ProgramCacheMap -> Maybe ProgramCacheValue
pCacheLookup (k3, k1) (k4, k2) m1 = TrieMap.lookup k1 m1 >>= TrieMap.lookup k2 >>= TrieMap.lookup k3 >>= TrieMap.lookup k4
pCacheInsert :: ShaderKey -> ShaderKey -> ProgramCacheValue -> ProgramCacheMap -> ProgramCacheMap 
pCacheInsert (k3, k1) (k4, k2) v m1 = TrieMap.alter' ins2 k1 m1
    where ins2 Nothing   = Just $ TrieMap.singleton k2 $ TrieMap.singleton k3 $ TrieMap.singleton k4 v
          ins2 (Just m2) = Just $ TrieMap.alter' ins3 k2 m2
          ins3 Nothing   = Just $ TrieMap.singleton k3 $ TrieMap.singleton k4 v
          ins3 (Just m3) = Just $ TrieMap.alter' ins4 k3 m3
          ins4 Nothing   = Just $ TrieMap.singleton k4 v
          ins4 (Just m4) = Just $ TrieMap.insert k4 v m4
          
type VBCache = HT.HashTable ([Int], StableName [[Float]]) VertexBuffer
type IBCache = HT.HashTable (StableName [Int]) IndexBuffer
data ContextCache = ContextCache {programCache :: ProgramCache,
                                  vbCache :: VBCache,
                                  ibCache :: IBCache,
                                  contextWindow :: GLUT.Window,
                                  contextViewPort :: Size}
newContextCache :: GLUT.Window -> IO ContextCache
newContextCache w = do
    pc <- newIORef TrieMap.empty
    vbc <- HT.new (==) (\(a,b) -> HT.hashString (map toEnum a) `xor` HT.hashInt (hashStableName b))
    ibc <- HT.new (==) (HT.hashInt . hashStableName)
    let cache = ContextCache pc vbc ibc w (Size 0 0)
    saveContextCache cache
    return cache

setContextWindow :: ContextCacheIO ()
setContextWindow = do w <- asks contextWindow
                      s <- asks contextViewPort
                      liftIO $ do GLUT.currentWindow $= Just w
                                  viewport $= (Position 0 0, s)                     


type ContextCacheIO = ReaderT ContextCache IO

createShaderResource :: Shader s => String -> IO s
createShaderResource str = do [s] <- genObjectNames 1
                              -- putStrLn $ "Created shader " ++ show s
                              shaderSource s $= [str]
                              compileShader s
                              b <- get $ compileStatus s
                              if b then return s
                                   else do e <- get $ shaderInfoLog s
                                           error $ e ++ "\nSource:\n\n" ++ str

createProgramResource :: ShaderKey -> String -> ShaderKey -> String -> Int -> ContextCacheIO (Program, (UniformLocationSet,UniformLocationSet))
createProgramResource vkey vstr fkey fstr s = do
                                       cache <- asks programCache
                                       m <- liftIO $ readIORef cache
                                       case pCacheLookup vkey fkey m of
                                            Just p -> return p
                                            Nothing -> liftIO $ do
                                                 [p] <- genObjectNames 1
                                                 -- putStrLn $ "Created program " ++ show p
                                                 vs <- createShaderResource vstr
                                                 fs <- createShaderResource fstr
                                                 attachedShaders p $= ([vs],[fs])
                                                 mapM_
                                                     (\i -> attribLocation p ("v" ++ show i) $= AttribLocation i)
                                                     [0..fromIntegral ((s-1) `div` 4)]
                                                 linkProgram p
                                                 b <- get $ linkStatus p
                                                 unless b $ do
                                                     e <- get $ programInfoLog p
                                                     error e
                                                 let allSamplers = [minBound..maxBound :: SamplerType]
                                                 fvu <- get $ uniformLocation p "vuf"
                                                 ivu <- get $ uniformLocation p "vui"
                                                 bvu <- get $ uniformLocation p "vub"
                                                 svus' <- mapM (\ s -> get $ uniformLocation p $ "vus" ++ show (fromEnum s)) allSamplers
                                                 let svus = Map.fromAscList $ zip allSamplers svus'
                                                 ffu <- get $ uniformLocation p "fuf"
                                                 ifu <- get $ uniformLocation p "fui"
                                                 bfu <- get $ uniformLocation p "fub"
                                                 sfus' <- mapM (\ s -> get $ uniformLocation p $ "fus" ++ show (fromEnum s)) allSamplers
                                                 let sfus = Map.fromAscList $ zip allSamplers sfus'
                                                 let p' = (p, ((fvu,ivu,bvu,svus),(ffu,ifu,bfu,sfus)))
                                                 atomicModifyIORef cache (flip (,) () . pCacheInsert vkey fkey p')
                                                 return p'

createVertexBuffer :: [[Float]] -> [Int] -> [[Float]] -> ContextCacheIO VertexBuffer
createVertexBuffer xs i v = do vName <- liftIO $ makeStableName v
                               let k = (i,vName)
                               cache <- asks vbCache
                               test <- liftIO $ HT.lookup cache k
                               case test of
                                  Just b -> return b
                                  Nothing -> do
                                     w <- asks contextWindow
                                     liftIO $ do [b] <- genObjectNames 1
                                                 -- putStrLn $ "Created vertex buffer " ++ show b
                                                 bindBuffer ArrayBuffer $= Just b
                                                 let xsdata = map realToFrac $ concat xs :: [CFloat]
                                                 withArray xsdata (\p -> bufferData ArrayBuffer $= (fromIntegral $ sizeOf (0 :: CFloat) * length xsdata, p, StaticDraw))
                                                 let b' = VertexBuffer b $ length (head xs)
                                                 HT.insert cache k b'
                                                 addFinalizer v $ do HT.delete cache k
                                                                     w' <- get GLUT.currentWindow
                                                                     GLUT.currentWindow $= Just w
                                                                     deleteObjectNames [b]
                                                                     GLUT.currentWindow $= w'
                                                                     -- putStrLn "Deleted vertex buffer"
                                                 return b'

createIndexBuffer :: [Int] -> Int -> ContextCacheIO IndexBuffer
createIndexBuffer xs vs = do cache <- asks ibCache
                             iName <- liftIO $ makeStableName xs
                             test <- liftIO $ HT.lookup cache iName
                             case test of
                                Just i -> return i
                                Nothing -> do
                                   w <- asks contextWindow
                                   liftIO $ do [b] <- genObjectNames 1
                                               -- putStrLn $ "Created index buffer " ++ show b
                                               bindBuffer ElementArrayBuffer $= Just b
                                               i <- if vs > fromIntegral (maxBound :: CUShort)
                                                then do
                                                    withArray (map fromIntegral  xs :: [CUInt]) (\p -> bufferData ElementArrayBuffer $= (fromIntegral $ sizeOf (0 :: CFloat) * length xs, p, StaticDraw))
                                                    return $ IndexBuffer b (length xs) UnsignedInt
                                                else if vs > fromIntegral (maxBound :: CUChar)
                                                 then do
                                                    withArray (map fromIntegral  xs :: [CUShort]) (\p -> bufferData ElementArrayBuffer $= (fromIntegral $ sizeOf (0 :: CFloat) * length xs, p, StaticDraw))
                                                    return $ IndexBuffer b (length xs) UnsignedShort
                                                 else do
                                                    withArray (map fromIntegral  xs :: [CUChar]) (\p -> bufferData ElementArrayBuffer $= (fromIntegral $ (sizeOf (0 :: CFloat)) * length xs, p, StaticDraw))
                                                    return $ IndexBuffer b (length xs) UnsignedByte
                                               HT.insert cache iName i
                                               addFinalizer xs $ do HT.delete cache iName
                                                                    w' <- get GLUT.currentWindow
                                                                    GLUT.currentWindow $= Just w
                                                                    deleteObjectNames [b]
                                                                    GLUT.currentWindow $= w'  
                                                                    -- putStrLn "Deleted index buffer"
                                               return i

useProgramResource :: Program -> ContextCacheIO ()
useProgramResource p = liftIO $ currentProgram $= Just p

useUniforms :: UniformLocationSet -> UniformSet -> ContextCacheIO ()
useUniforms (fu,iu,bu,su) (f,i,b,s) = do
            w <- asks contextWindow
            liftIO $ do
                unless (null f) $ withArray (map (TexCoord1 . realToFrac) f :: [TexCoord1 GLfloat]) $ uniformv fu (fromIntegral $ length f)
                unless (null i) $ withArray (map (TexCoord1 . fromIntegral) i :: [TexCoord1 GLint]) $ uniformv iu (fromIntegral $ length i)
                unless (null b) $ withArray (map (TexCoord1 . fromBool) b :: [TexCoord1 GLint]) $ uniformv bu (fromIntegral $ length b)
                mapM_ (useSampler w) $ Map.toList s
    where
        useSampler w (t, xs) = do
            let texs = nub xs
            samplers <- mapM (createSampler w t) $ zip texs [0..]
            let texToSamp = zip texs samplers
                ss = map (fromJust . flip lookup texToSamp) xs
            withArray ss $ uniformv (su Map.! t) (fromIntegral $ length ss)
        createSampler w t ((Sampler f e,tex),i) = do
            activeTexture $= TextureUnit i
            bindWinMappedTexture (target t) w tex t
            textureFilter (target t) $= toGLFilter f
            mapM_ (\c -> textureWrapMode (target t) c $= toGLWrap e) [S, T, R]
            return $ TexCoord1 (fromIntegral i::GLint)
        target Sampler3D = Texture3D
        target Sampler2D = Texture2D
        target Sampler1D = Texture1D
        target SamplerCube = TextureCubeMap

useVertexBuffer :: VertexBuffer -> IO ()
useVertexBuffer (VertexBuffer b s) = do bindBuffer ArrayBuffer $= Just b
                                        mapM_
                                            (\i -> do
                                                let ptroffset = nullPtr `plusPtr` (sizeOf (0 :: CFloat) * i * 4)
                                                    stride = fromIntegral $ sizeOf (0 :: CFloat) * s
                                                    a = AttribLocation $ fromIntegral i
                                                    e = fromIntegral $ min (s - i*4) 4
                                                vertexAttribArray a $= Enabled
                                                vertexAttribPointer a $= (ToFloat, VertexArrayDescriptor e Float stride ptroffset)
                                            )
                                            [0..(s-1) `div` 4]


drawIndexVertexBuffer :: PrimitiveMode -> VertexBuffer -> IndexBuffer -> IO ()
drawIndexVertexBuffer p vb (IndexBuffer i s t) = do useVertexBuffer vb
                                                    bindBuffer ElementArrayBuffer $= Just i
                                                    drawElements p (fromIntegral s) t nullPtr

drawVertexBuffer :: PrimitiveMode -> VertexBuffer -> Int -> IO ()
drawVertexBuffer p vb s = do useVertexBuffer vb
                             drawArrays p 0 (fromIntegral s)


{-# NOINLINE hiddenWindowContextCache #-}
hiddenWindowContextCache :: ContextCache
hiddenWindowContextCache = unsafePerformIO $ do
       GLUT.initialDisplayMode $= [ GLUT.SingleBuffered, GLUT.RGBMode, GLUT.WithAlphaComponent, GLUT.WithDepthBuffer, GLUT.WithStencilBuffer ]
       w <- GLUT.createWindow "Hidden Window"
       GLUT.windowStatus $= GLUT.Hidden
       newContextCache w

{-# NOINLINE windowContextCaches #-}
windowContextCaches :: IORef (Map GLUT.Window ContextCache)
windowContextCaches = unsafePerformIO $ newIORef $ Map.empty

getContextCache :: GLUT.Window -> IO (Maybe ContextCache)
getContextCache w = do m <- atomicModifyIORef windowContextCaches (\m -> (m,m))
                       return $ Map.lookup w m

saveContextCache :: ContextCache -> IO ()
saveContextCache c = atomicModifyIORef windowContextCaches $ \ m -> (Map.insert (contextWindow c) c m, ())

changeContextSize :: GLUT.Window -> Size -> IO ()
changeContextSize w s = atomicModifyIORef windowContextCaches $ \ m -> (Map.adjust (\c -> c {contextViewPort = s}) w m, ())

getCurrentOrSetHiddenContext = do
    mw <- get GLUT.currentWindow
    case mw of Just w  -> do mc <- getContextCache w
                             case mc of Just cache -> return cache
                                        Nothing    -> setAndGetHiddenWindow
               Nothing -> setAndGetHiddenWindow
    where
        setAndGetHiddenWindow = do GLUT.currentWindow $= Just (contextWindow hiddenWindowContextCache)
                                   return hiddenWindowContextCache

evaluateDeep a = do evaluate (a==a)
                    return a

ioEvaluate :: Eq a => a -> ContextCacheIO a
ioEvaluate = liftIO . evaluateDeep

evaluatePtr p = do a <- peek (castPtr p :: Ptr CUChar)
                   evaluate (a==a)
                   return p

----------------------------------------------------
-- Texture operations


type WinMappedTexture = IORef (Map GLUT.Window TextureObject)

newWinMappedTexture :: (TextureObject -> ContextCache -> IO a) -> IO WinMappedTexture
newWinMappedTexture ionew = do
    cache <- getCurrentOrSetHiddenContext
    [tex] <- genObjectNames 1
    -- putStrLn $ "Created texture " ++ show tex
    swapBytes Unpack $= False
    lsbFirst Unpack $= False
    rowLength Unpack $= 0
    skipRows Unpack $= 0
    skipPixels Unpack $= 0
    rowAlignment Unpack $= 1  -- Set to no padding
    imageHeight Unpack $= 0
    skipImages Unpack $= 0
    ionew tex cache
    ref <- newIORef $ Map.singleton (contextWindow cache) tex
    mkWeakIORef ref $ do m <- readIORef ref
                         w <- get GLUT.currentWindow
                         mapM_ deleteTexture $ Map.toList m
                         GLUT.currentWindow $= w
                         -- putStrLn "Deleted texture"
    return ref
    where
        deleteTexture (w,t) = do GLUT.currentWindow $= Just w
                                 deleteObjectNames [t]
                                   
bindWinMappedTexture target w ref s  = do
    mtex <- atomicModifyIORef ref (\a -> (a, takeOne a))
    case mtex of
        Right tex -> textureBinding target $= Just tex
        Left (w',t) -> do GLUT.currentWindow $= Just w'
                          textureBinding target $= Just t
                          ft <- get $ textureLevelRange target
                          f <- get $ textureInternalFormat (Left target) 0
                          swapBytes Unpack $= False
                          lsbFirst Unpack $= False
                          rowLength Unpack $= 0
                          skipRows Unpack $= 0
                          skipPixels Unpack $= 0
                          rowAlignment Unpack $= 1  -- Set to no padding
                          imageHeight Unpack $= 0
                          skipImages Unpack $= 0
                          swapBytes Pack $= False
                          lsbFirst Pack $= False
                          rowLength Pack $= 0
                          skipRows Pack $= 0
                          skipPixels Pack $= 0
                          rowAlignment Pack $= 1  -- Set to no padding
                          imageHeight Pack $= 0
                          skipImages Pack $= 0
                          tex <- transferTexture s ft f
                          textureLevelRange target $= ft
                          -- putStrLn $ "Transferred texture " ++ show tex
                          atomicModifyIORef ref (flip (,) () . Map.insertWith (const id) w tex)
    where takeOne a = case Map.lookup w a of
                        Nothing -> Left $ Map.elemAt 0 a
                        Just t -> Right t
          createTexInWin = do GLUT.currentWindow $= Just w
                              [tex] <- genObjectNames 1
                              textureBinding target $= Just tex
                              return tex
          transferTexture Sampler3D (from,to) f = do
                psSize <- mapM getDataAndSize3D [from..to]
                tex <- createTexInWin
                mapM_ (setDataWithSize3D f) $ zip psSize [from..to]
                return tex
          transferTexture Sampler2D (from,to) f = do
                psSize <- mapM getDataAndSize2D [from..to]
                tex <- createTexInWin
                mapM_ (setDataWithSize2D f) $ zip psSize [from..to]
                return tex
          transferTexture Sampler1D (from,to) f = do
                psSize <- mapM getDataAndSize1D [from..to]
                tex <- createTexInWin
                mapM_ (setDataWithSize1D f) $ zip psSize [from..to]
                return tex
          transferTexture SamplerCube (from,to) f =  do
                psSize <- mapM getDataAndSizeCube [(n,side) | n <- [from..to], side <- cubeMapTargets]
                tex <- createTexInWin
                mapM_ (setDataWithSizeCube f) $ zip psSize [(n,side) | n <- [from..to], side <- cubeMapTargets]
                return tex

          getDataAndSize3D n = do
                s@(TextureSize3D x y z) <- get $ textureSize3D (Left Texture3D) n
                fp <- mallocForeignPtrBytes (fromIntegral x * fromIntegral y * fromIntegral z * 4 * sizeOf (undefined :: Float))
                withForeignPtr fp $ \ p -> getTexImage (Left Texture3D) n (PixelData RGBA Float p)
                return (s,fp)
          setDataWithSize3D f ((s,fp),n) =
                withForeignPtr fp $ \ p -> texImage3D NoProxy n f s 0 (PixelData RGBA Float p)
          getDataAndSize2D n = do
                s@(TextureSize2D x y) <- get $ textureSize2D (Left Texture2D) n
                fp <- mallocForeignPtrBytes (fromIntegral x * fromIntegral y * 4 * sizeOf (undefined :: Float))
                withForeignPtr fp $ \ p -> getTexImage (Left Texture2D) n (PixelData RGBA Float p)
                return (s,fp)
          setDataWithSize2D f ((s,fp),n) =
                withForeignPtr fp $ \ p -> texImage2D Nothing NoProxy n f s 0 (PixelData RGBA Float p)
          getDataAndSize1D n = do
                s@(TextureSize1D x) <- get $ textureSize1D (Left Texture1D) n
                fp <- mallocForeignPtrBytes (fromIntegral x * 4 * sizeOf (undefined :: Float))
                withForeignPtr fp $ \ p -> getTexImage (Left Texture2D) n (PixelData RGBA Float p)
                return (s,fp)
          setDataWithSize1D f ((s,fp),n) =
                withForeignPtr fp $ \ p -> texImage1D NoProxy n f s 0 (PixelData RGBA Float p)
          getDataAndSizeCube (n,side) = do
                s@(TextureSize2D x y) <- get $ textureSize2D (Right side) n
                fp <- mallocForeignPtrBytes (fromIntegral x * fromIntegral y * 4 * sizeOf (undefined :: Float))
                withForeignPtr fp $ \ p -> getTexImage (Right side) n (PixelData RGBA Float p)
                return (s,fp)
          setDataWithSizeCube f ((s,fp),(n,side)) =
                withForeignPtr fp $ \ p -> texImage2D (Just side) NoProxy n f s 0 (PixelData RGBA Float p)

cubeMapTargets = [TextureCubeMapPositiveX, TextureCubeMapNegativeX, TextureCubeMapPositiveY, TextureCubeMapNegativeY, TextureCubeMapPositiveZ, TextureCubeMapNegativeZ]
