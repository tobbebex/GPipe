{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, EmptyDataDecls, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  Shader
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

module Shader (
    GPU(..),
    rasterizeVertex,
    inputVertex,
    fragmentFrontFacing,
    Shader(),
    V, 
    F,
    Vertex,
    Fragment,
    ShaderInfo,
    getShaders,
    Real'(..),
    Convert(..),
    dFdx,
    dFdy,
    fwidth,
    fragDepth,
    sampleBinFunc,
    sampleTernFunc,
    module Data.Boolean
) where

import Control.Monad.Trans.State.Lazy (put, get, StateT, runStateT)
import System.IO.Unsafe
import Data.Vec ((:.)(..), Vec2, Vec3, Vec4, norm, normalize, dot, cross)
import qualified Data.Vec as Vec
import Data.Unique
import Data.List
import Data.Maybe
import Data.Boolean
import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)
import qualified Data.HashTable.IO as HT
import Control.Exception (evaluate)
import System.Mem.StableName
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet hiding (IntSet)
import Control.Arrow (first, second)
import Resources
import Formats

infixl 7 `mod'`

-- | Denotes a type on the GPU, that can be moved there from the CPU (through the internal use of uniforms).
--   Use the existing instances of this class to create new ones.
class GPU a where
    -- | The type on the CPU.
    type CPU a
    -- | Converts a value from the CPU to the GPU.
    toGPU :: CPU a -> a

data ShaderTree = ShaderUniform !Uniform 
                | ShaderConstant !Const
                | ShaderInput !Int
                | ShaderInputTree ShaderTree
                | ShaderOp !Op (String -> [String] -> String) [ShaderTree]
type ShaderDAG = ([Int],[(ShaderTree, [Int])])

-- | An opaque type constructor for atomic values in a specific GPU context (i.e. 'V' or 'F'), e.g. 'Shader' 'V' 'Float'.
newtype Shader c t = Shader { fromS :: ShaderTree }

-- | Used to denote a vertex context in the first parameter to 'Shader'
data V
-- | Used to denote a fragment context in the first parameter to 'Shader'
data F 

-- | A type synonyme for atomic values in a vertex on the GPU, e.g. 'Vertex' 'Float'.
type Vertex = Shader V
-- | A type synonyme for atomic values in a fragment on the GPU, e.g. 'Fragment' 'Float'. 
type Fragment = Shader F

rasterizeVertex :: Vertex Float -> Fragment Float
rasterizeVertex = Shader . ShaderInputTree . fromS
inputVertex :: Int -> Vertex Float
inputVertex = Shader . ShaderInput
fragmentFrontFacing :: Fragment Bool
fragmentFrontFacing = Shader $ ShaderOp "gl_ff" (assign bool (const "gl_FrontFacing")) []

getShaders :: Vec4 (Vertex Float) -> Fragment Bool -> Vec4 (Fragment Float) -> Maybe (Fragment Float) -> (ShaderInfo, ShaderInfo, [Int])
getShaders pos (Shader ndisc) color mdepth = ((createShaderKey vdag,vstr,vuns),(createShaderKey fdag,fstr,funs), inputs)
    where fcolor = fromS $ fromVec "vec4" color
          (varyings, fdag@(fcolor':ndisc':mdepth',_)) = splitShaders (createDAG (fcolor:ndisc: map fromS (maybeToList mdepth)))
          vpos = fromS $ fromVec "vec4" pos
          vdag@(vpos':varyings',_) = createDAG (vpos:varyings)
          inputs = extractInputs vdag
          vcodeAssigns = getCodeAssignments (fromJust . flip elemIndex inputs) (length inputs) "v" vdag
          vCodeFinish = setVaryings varyings' ++
                        "gl_Position = t" ++ show vpos' ++ ";\n"
          fcodeAssigns = getCodeAssignments id (length varyings') "f" fdag
          depthAssign = case mdepth' of [d] -> "gl_FragDepth = t" ++ show d ++ ";\n"
                                        []  -> ""
          fcodeFinish = "if (!t" ++ show ndisc' ++ ") discard;\n" ++
                        depthAssign ++
                        "gl_FragColor = t" ++ show fcolor' ++ ";\n"
          vuns = extractUniforms vdag
          funs = extractUniforms fdag
          attributeDecl = inoutDecls "attribute" "v" (length inputs)
          varyingDecl = inoutDecls "varying" "f" (length varyings')
          vstr = makeShader (attributeDecl ++ varyingDecl ++ uniformDecls "v" vuns) (vcodeAssigns ++ vCodeFinish)
          fstr = makeShader (varyingDecl ++ uniformDecls "f" funs) (fcodeAssigns ++ fcodeFinish)          
                
sampleBinFunc f t s tex c = toColor $ toVec float 4 (binaryFunc "vec4" f (Shader $ ShaderUniform $ UniformSampler t s tex) (fromVec (tName c) c))
sampleTernFunc f t s tex c x = toColor $ toVec float 4 (ternaryFunc "vec4" f (Shader $ ShaderUniform $ UniformSampler t s tex) (fromVec (tName c) c) x)

instance GPU (Shader c Float) where
    type CPU (Shader c Float) = Float
    toGPU = Shader . ShaderUniform . UniformFloat
instance GPU (Shader c Int) where
    type CPU (Shader c Int) = Int
    toGPU = Shader . ShaderUniform . UniformInt
instance GPU (Shader c Bool) where
    type CPU (Shader c Bool) = Bool
    toGPU = Shader . ShaderUniform . UniformBool

instance GPU () where
    type CPU () = ()
    toGPU = id
instance (GPU a, GPU b) => GPU (a,b) where
    type CPU (a,b) = (CPU a, CPU b)
    toGPU (a,b)= (toGPU a, toGPU b)
instance (GPU a, GPU b, GPU c) => GPU (a,b,c) where
    type CPU (a,b,c) = (CPU a, CPU b, CPU c)
    toGPU (a,b,c)= (toGPU a, toGPU b, toGPU c)
instance (GPU a, GPU b, GPU c, GPU d) => GPU (a,b,c,d) where
    type CPU (a,b,c,d) = (CPU a, CPU b, CPU c, CPU d)
    toGPU (a,b,c,d)= (toGPU a, toGPU b, toGPU c, toGPU d)

instance (GPU a, GPU b) => GPU (a:.b) where
    type CPU (a:.b) = CPU a :. CPU b
    toGPU (a:.b) = toGPU a :. toGPU b

 
instance Num (Shader c Float) where
  negate      = unaryPreOp float "-"
  (+)         = binaryOp float "+"
  (*)         = binaryOp float "*"
  fromInteger = Shader . ShaderConstant . ConstFloat . fromInteger
  abs         = unaryFunc float "abs"
  signum      = unaryFunc float "sign"
  
  
instance Num (Shader c Int) where
  negate      = unaryPreOp int "-"
  (+)         = binaryOp int "+"
  (*)         = binaryOp int "*"
  fromInteger = Shader . ShaderConstant . ConstInt . fromInteger
  abs x       = ifB (x <* 0) (-x) x
  signum x    = ifB (x <* 0) (-1) 1
    
instance Fractional (Shader c Float) where
  (/)          = binaryOp float "/"
  fromRational = Shader . ShaderConstant . ConstFloat . fromRational
instance Floating (Shader c Float) where
  pi    = Shader $ ShaderConstant $ ConstFloat pi
  sqrt  = unaryFunc float "sqrt"
  exp   = unaryFunc float "exp"
  log   = unaryFunc float "log"
  (**)  = binaryFunc float "pow"
  sin   = unaryFunc float "sin"
  cos   = unaryFunc float "cos"
  tan   = unaryFunc float "tan"
  asin  = unaryFunc float "asin"
  acos  = unaryFunc float "acos"
  atan  = unaryFunc float "atan"
  sinh x = (exp x - exp (-x)) / 2 
  cosh x = (exp x + exp (-x)) / 2
  asinh x = log (x + sqrt (x * x + 1))
  atanh x = log ((1 + x) / (1 - x)) / 2
  acosh x = log (x + sqrt (x * x - 1))
 
-- | This class provides the GPU functions either not found in Prelude's numerical classes, or that has wrong types.
--   Instances are also provided for normal 'Float's and 'Double's.
--   Minimal complete definition: 'floor'' and 'ceiling''.
class Floating a => Real' a where
  rsqrt :: a -> a
  exp2 :: a -> a
  log2 :: a -> a
  floor' :: a -> a
  ceiling' :: a -> a
  fract' :: a -> a
  mod' :: a -> a -> a
  clamp :: a -> a -> a -> a
  saturate :: a -> a
  mix :: a -> a -> a-> a
  step :: a -> a -> a
  smoothstep :: a -> a -> a -> a

  rsqrt = (1/) . sqrt
  exp2 = (2**)
  log2 = logBase 2
  saturate x = clamp x 0 1
  mix x y a = x*(1-a)+y*a
  smoothstep a b x = let t = saturate ((x-a) / (b-a))
                     in t*t*(3-2*t)
  fract' x = x - floor' x
  mod' x y = x - y* floor' (x/y)
  
instance Real' Float where
  clamp x a = min (max x a)
  step a x | x < a     = 0
           | otherwise = 1
  floor' = fromIntegral . floor
  ceiling' = fromIntegral . ceiling

instance Real' Double where
  clamp x a = min (max x a)
  step a x | x < a     = 0
           | otherwise = 1
  floor' = fromIntegral . floor
  ceiling' = fromIntegral . ceiling
  
instance Real' (Shader c Float) where
  rsqrt = unaryFunc float "inversesqrt"
  exp2 = unaryFunc float "exp2"
  log2 = unaryFunc float "log2"
  floor' = unaryFunc float "floor"
  ceiling' = unaryFunc float "ceil"
  fract' = unaryFunc float "fract"
  mod' = binaryFunc float "mod"
  clamp = ternaryFunc float "clamp"
  mix = ternaryFunc float "mix"
  step = binaryFunc float "step"
  smoothstep = ternaryFunc float "smoothstep"

type instance BooleanOf (Shader c a) = (Shader c Bool)
  
instance Boolean (Shader c Bool) where
    true = Shader $ ShaderConstant $ ConstBool True
    false = Shader $ ShaderConstant $ ConstBool False
    notB = unaryPreOp bool "!"
    (&&*) = binaryOp bool "&&"
    (||*) = binaryOp bool "||"
instance Eq a => EqB (Shader c a) where
    (==*) = binaryOp bool "=="
    (/=*) = binaryOp bool "!="
instance Ord a => OrdB (Shader c a) where
    (<*) = binaryOp bool "<"
    (>=*) = binaryOp bool ">="
    (>*) = binaryOp bool ">"
    (<=*) = binaryOp bool "<="

instance IfB (Shader c Int) where
    ifB c a b = Shader $ ShaderOp "if" (assign int (\[a,b,c]->a++"?"++b++":"++c)) [fromS c,fromS a,fromS b]
instance IfB (Shader c Float) where
    ifB c a b = Shader $ ShaderOp "if" (assign float (\[a,b,c]->a++"?"++b++":"++c)) [fromS c,fromS a,fromS b]
instance IfB (Shader c Bool) where
    ifB c a b = Shader $ ShaderOp "if" (assign bool (\[a,b,c]->a++"?"++b++":"++c)) [fromS c,fromS a,fromS b]
    
-- | Provides a common way to convert numeric types to integer and floating point representations.
class Convert a where
    type ConvertFloat a
    type ConvertInt a
    -- | Convert to a floating point number.
    toFloat :: a -> ConvertFloat a
    -- | Convert to an integral number, using truncation if necessary.
    toInt :: a -> ConvertInt a

instance Convert Float where
    type ConvertFloat Float = Float
    type ConvertInt Float = Int
    toFloat = id
    toInt = truncate
instance Convert Int where
    type ConvertFloat Int = Float
    type ConvertInt Int = Int
    toFloat = fromIntegral
    toInt = id
instance Convert (Shader c Float) where
    type ConvertFloat (Shader c Float) = Shader c Float
    type ConvertInt (Shader c Float) = Shader c Int
    toFloat = id
    toInt = unaryFunc int int
instance Convert (Shader c Int) where
    type ConvertFloat (Shader c Int) = Shader c Float
    type ConvertInt (Shader c Int) = Shader c Int
    toFloat = unaryFunc float float
    toInt = id
    
-- | The derivative in x using local differencing of the rasterized value.
dFdx :: Fragment Float -> Fragment Float
-- | The derivative in y using local differencing of the rasterized value.
dFdy :: Fragment Float -> Fragment Float
-- | The sum of the absolute derivative in x and y using local differencing of the rasterized value.
fwidth :: Fragment Float -> Fragment Float
-- | The local fragment depth value
fragDepth :: Fragment Float

dFdx = unaryFunc float "dFdx"
dFdy = unaryFunc float "dFdy"
fwidth = unaryFunc float "fwidth"
fragDepth = shaderVar float "gl_FragCoord.z"

--------------------------------------
-- Vector specializations

{-# RULES "norm/F4" norm = normF4 #-}
{-# RULES "norm/F3" norm = normF3 #-}
{-# RULES "norm/F2" norm = normF2 #-}
normF4 :: Vec4 (Shader c  Float) -> Shader c  Float
normF4 = unaryFunc float "length" . fromVec "vec4"
normF3 :: Vec3 (Shader c  Float) -> Shader c  Float
normF3 = unaryFunc float "length" . fromVec "vec3"
normF2 :: Vec2 (Shader c  Float) -> Shader c  Float
normF2 = unaryFunc float "length" . fromVec "vec2"

{-# RULES "normalize/F4" normalize = normalizeF4 #-}
{-# RULES "normalize/F3" normalize = normalizeF3 #-}
{-# RULES "normalize/F2" normalize = normalizeF2 #-}
normalizeF4 :: Vec4 (Shader c  Float) -> Vec4 (Shader c  Float)
normalizeF4 = toVec float 4 . unaryFunc "vec4" "normalize" . fromVec "vec4"
normalizeF3 :: Vec3 (Shader c  Float) -> Vec3 (Shader c  Float)
normalizeF3 = toVec float 3 . unaryFunc "vec3" "normalize" . fromVec "vec3"
normalizeF2 :: Vec2 (Shader c  Float) -> Vec2 (Shader c  Float)
normalizeF2 = toVec float 2 . unaryFunc "vec2" "normalize" . fromVec "vec2"

{-# RULES "dot/F4" dot = dotF4 #-}
{-# RULES "dot/F3" dot = dotF3 #-}
{-# RULES "dot/F2" dot = dotF2 #-}
dotF4 :: Vec4 (Shader c  Float) -> Vec4 (Shader c  Float) -> Shader c  Float
dotF4 a b = binaryFunc float "dot" (fromVec "vec4" a) (fromVec "vec4" b)
dotF3 :: Vec3 (Shader c  Float) -> Vec3 (Shader c  Float) -> Shader c  Float
dotF3 a b = binaryFunc float "dot" (fromVec "vec3" a) (fromVec "vec3" b)
dotF2 :: Vec2 (Shader c  Float) -> Vec2 (Shader c  Float) -> Shader c  Float
dotF2 a b = binaryFunc float "dot" (fromVec "vec2" a) (fromVec "vec2" b)

{-# RULES "cross/F3" cross = crossF3 #-}
crossF3 :: Vec3 (Shader c  Float) -> Vec3 (Shader c  Float) -> Vec3 (Shader c  Float)
crossF3 a b = toVec float 3 $ binaryFunc "vec3" "cross" (fromVec "vec3" a) (fromVec "vec3" b)


{-# RULES "minB/F" minB = minS #-}
{-# RULES "maxB/F" maxB = maxS #-}
minS :: Shader a Float -> Shader a Float -> Shader a Float 
minS = binaryFunc float "min"
maxS :: Shader a Float -> Shader a Float -> Shader a Float 
maxS = binaryFunc float "max"

--------------------------------------
-- Private
--

setVaryings xs = setVaryings' 0 $ map (('t':) . show) xs
    where 
        setVaryings' _ [] = ""
        setVaryings' n xs = case splitAt 4 xs of (ys,rest) -> "f" ++ show n ++ " = " ++ tName' (length ys) ++ "(" ++ intercalate "," ys ++ ");\n" ++ setVaryings' (n+1) rest

inoutDecls t n i = inoutDecls' i 0 
    where inoutDecls' i x | i >= 4    = t ++ " vec4 " ++ n ++ show x ++ ";\n" ++ inoutDecls' (i-4) (x+1)
                          | i == 0    = ""
                          | otherwise = t ++ " " ++ tName' i ++ " " ++ n ++ show x ++ ";\n"
          
uniformDecls :: String -> UniformSet -> String
uniformDecls p (f,i,b,s) = makeU float "f" (length f) ++
                           makeU int "i" (length i) ++
                           makeU bool "b" (length b) ++
                           concatMap (\(t,xs) -> makeU (sampName t) ('s':show (fromEnum t)) (length xs)) (Map.toList s)
    where makeU t n 0 = ""
          makeU t n i = "uniform " ++ t ++ " " ++ p ++ "u" ++ n ++ "[" ++ show i ++ "];\n"
                                                           
makeShader init assignments = "#version 120\n" ++
                     init ++
                     "void main(){\n" ++
                     assignments ++
                     "}\n"
                     
createShaderKey :: ShaderDAG -> ShaderKey
createShaderKey (a,xs) = (a,map (first toShaderKeyNode) xs)
    where toShaderKeyNode (ShaderUniform _) = ShaderKeyUniform
          toShaderKeyNode (ShaderInput a) = ShaderKeyInput a
          toShaderKeyNode (ShaderConstant a) = ShaderKeyConstant a
          toShaderKeyNode (ShaderOp a _ _) = ShaderKeyOp a
          toShaderKeyNode (ShaderInputTree _) = error "Use splitShaders first"

splitShaders :: ShaderDAG -> ([ShaderTree], ShaderDAG) -- ^ (previous, current)
splitShaders (a,xs) = case mapAccumL splitNode [] xs of (trees, xs2) -> (reverse trees, (a,xs2))
    where splitNode ts (ShaderInputTree a, ys) = (a:ts, (ShaderInput (length ts), ys))
          splitNode ts a =  (ts, a)

createDAG :: [ShaderTree] -> ShaderDAG
createDAG = second reverse . unsafePerformIO . startDAG
    where startDAG xs = do ht <- HT.new
                           runStateT (mapM (createDAG' ht) xs) []
          createDAG' :: HT.BasicHashTable (StableName ShaderTree) Int -> ShaderTree -> StateT [(ShaderTree, [Int])] IO Int
          createDAG' ht n = do n' <- liftIO $ evaluate n -- To make makeStableName "stable"
                               k <- liftIO $ makeStableName n'
                               m <- liftIO $ HT.lookup ht k
                               case m of
                                  Just i -> return i
                                  Nothing -> do xs' <- case n' of 
                                                         ShaderOp _ _ xs -> mapM (createDAG' ht) xs
                                                         _ -> return []
                                                ys <- get
                                                let y = length ys
                                                liftIO $ HT.insert ht k y
                                                put $ (n',xs'):ys
                                                return y



extractUniforms :: ShaderDAG -> UniformSet 
extractUniforms (_,xs) = foldl' extractUniform ([],[],[],Map.empty) $ reverse $ map fst xs
    where extractUniform (a,b,c,m) (ShaderUniform (UniformFloat x)) = (x:a,b,c,m)
          extractUniform (a,b,c,m) (ShaderUniform (UniformInt x)) = (a,x:b,c,m)
          extractUniform (a,b,c,m) (ShaderUniform (UniformBool x)) = (a,b,x:c,m)
          extractUniform (a,b,c,m) (ShaderUniform (UniformSampler t s tex)) = (a,b,c,Map.insertWith' (++) t [(s,tex)] m)
          extractUniform x _ = x  

extractInputs :: ShaderDAG -> [Int]
extractInputs (_,xs) = IntSet.toAscList $ foldl' extractIn IntSet.empty $ map fst xs
    where extractIn s (ShaderInput a) = IntSet.insert a s
          extractIn x _ = x  

getCodeAssignments :: (Int -> Int) -> Int -> String -> ShaderDAG -> String
getCodeAssignments inF numIns inName (_,xs) = concat $ snd $ mapAccumL getCode ((0,0,0,Map.empty),Map.empty) $ zip [0..] xs
    where getCode ((f,i,b,s),inlns) (n, (ShaderUniform (UniformFloat _), _)) = (((f+1,i,b,s),inlns), assign float (const $ inName ++ "uf[" ++ show f ++ "]") (var n) [])
          getCode ((f,i,b,s),inlns) (n, (ShaderUniform (UniformInt _), _)) = (((f,i+1,b,s),inlns), assign int (const $ inName ++ "ui[" ++ show i ++ "]") (var n) [])
          getCode ((f,i,b,s),inlns) (n, (ShaderUniform (UniformBool _), _)) = (((f,i,b+1,s),inlns), assign bool (const $ inName ++ "ub[" ++ show b ++ "]") (var n) [])
          getCode ((f,i,b,s),inlns) (n, (ShaderUniform (UniformSampler t _ _), _)) =
                case first (fromMaybe 0) $ Map.insertLookupWithKey (const $ const (+1)) t 1 s of
                    (x, s') -> (((f,i,b,s'),Map.insert n (inName ++ "us" ++ show (fromEnum t) ++ "[" ++ show x ++ "]") inlns), "") 
          getCode x (n, (ShaderConstant (ConstFloat f), _)) = (x, assign float (const $ show f) (var n) [])
          getCode x (n, (ShaderConstant (ConstInt i), _)) = (x, assign int (const $ show i) (var n) [])
          getCode x (n, (ShaderConstant (ConstBool b), _)) = (x, assign bool (const $ if b then "true" else "false") (var n) [])
          getCode x (n, (ShaderInput i, _)) = (x, assign float (const $ inName ++ inoutAccessor (inF i) numIns) (var n) [])
          getCode x@(_,inlns) (n, (ShaderOp _ f _, xs)) = (x, f (var n) (map (varMaybeInline inlns) xs))
          getCode _ (_, (ShaderInputTree _, _)) = error "Shader.getCodeAssignments: Use splitShaders first!"
          var n = 't' : show n
          varMaybeInline inlns n = fromMaybe (var n) (Map.lookup n inlns)

inoutAccessor i tot = case divMod i 4 of (d,m) -> if i+1 == tot && m == 0 then show d else show d ++ "." ++ (["x","y","z","w"]!!m)

sampName Sampler3D = "sampler3D"
sampName Sampler2D = "sampler2D"
sampName Sampler1D = "sampler1D"
sampName SamplerCube = "samplerCube"

tName v = tName' $ Vec.length v
tName' 1 = float
tName' x = "vec" ++ show x

assign :: String -> ([String] -> String) -> String -> [String] -> String
assign t f x ys = t ++ " " ++ x ++ "=" ++ f ys ++ ";\n"
binFunc :: String -> [String] -> String
binFunc s = head . binFunc'
    where
        binFunc' (a:b:xs) = binFunc' $ (s ++ "(" ++ a ++ "," ++ b ++ ")"):binFunc' xs
        binFunc' x = x

binaryOp t s a b = Shader $ ShaderOp s (assign t (intercalate s)) [fromS a, fromS b]
unaryPreOp t s a = Shader $ ShaderOp s (assign t ((s ++) . head)) [fromS a]
unaryPostOp t s a = Shader $ ShaderOp s (assign t ((++ s) . head)) [fromS a]
unaryFunc t s a = Shader $ ShaderOp s (assign t (((s ++ "(") ++) . (++ ")") . head)) [fromS a]
binaryFunc t s a b = Shader $ ShaderOp s (assign t (binFunc s)) [fromS a, fromS b]
ternaryFunc t s a b c = Shader $ ShaderOp s (assign t (\[a,b,c]->s++"("++a++","++b++","++c++")")) [fromS a, fromS b, fromS c]
fromVec t = Shader . ShaderOp "" (assign t (((t ++ "(") ++) . (++ ")") . intercalate ",")) . map fromS . Vec.toList 
toVec t n a = Vec.fromList $ map (\s -> Shader $ ShaderOp s (assign t (\[x]->x++"["++s++"]")) [fromS a]) [show n' | n' <-[0..n - 1]]
shaderVar t s = Shader $ ShaderOp s (assign t (const s)) []

float = "float"
int = "int"
bool = "bool"