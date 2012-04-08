{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
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
    Vertex(),
    Fragment(),
    ShaderInfo,
    getShaders,
    Real'(..),
    Convert(..),
    dFdx,
    dFdy,
    fwidth,
    vSampleBinFunc,
    fSampleBinFunc,
    vSampleTernFunc,
    fSampleTernFunc,
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
import qualified Data.HashTable as HT
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

-- | An opaque type constructor for atomic values in a vertex on the GPU, e.g. 'Vertex' 'Float'.
newtype Vertex a = Vertex { fromVertex :: ShaderTree }
-- | An opaque type constructor for atomic values in a fragment on the GPU, e.g. 'Fragment' 'Float'. 
newtype Fragment a = Fragment { fromFragment :: ShaderTree }

rasterizeVertex :: Vertex Float -> Fragment Float
rasterizeVertex = Fragment . ShaderInputTree . fromVertex
inputVertex :: Int -> Vertex Float
inputVertex = Vertex . ShaderInput
fragmentFrontFacing :: Fragment Bool
fragmentFrontFacing = Fragment $ ShaderOp "gl_ff" (assign "bool" (const "gl_FrontFacing")) []

getShaders :: Vec4 (Vertex Float) -> Fragment Bool -> Vec4 (Fragment Float) -> Maybe (Fragment Float) -> (ShaderInfo, ShaderInfo, [Int])
getShaders pos (Fragment ndisc) color mdepth = ((createShaderKey vdag,vstr,vuns),(createShaderKey fdag,fstr,funs), inputs)
    where fcolor = fromFragment $ fFromVec "vec4" color
          (varyings, fdag@(fcolor':ndisc':mdepth',_)) = splitShaders (createDAG (fcolor:ndisc: map fromFragment (maybeToList mdepth)))
          vpos = fromVertex $ vFromVec "vec4" pos
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
                
vSampleBinFunc f t s tex c = toColor $ vToVec "float" 4 (vBinaryFunc "vec4" f (Vertex $ ShaderUniform $ UniformSampler t s tex) (vFromVec (tName c) c))
fSampleBinFunc f t s tex c = toColor $ fToVec "float" 4 (fBinaryFunc "vec4" f (Fragment $ ShaderUniform $ UniformSampler t s tex) (fFromVec (tName c) c))
vSampleTernFunc f t s tex c x = toColor $ vToVec "float" 4 (vTernaryFunc "vec4" f (Vertex $ ShaderUniform $ UniformSampler t s tex) (vFromVec (tName c) c) x)
fSampleTernFunc f t s tex c x = toColor $ fToVec "float" 4 (fTernaryFunc "vec4" f (Fragment $ ShaderUniform $ UniformSampler t s tex) (fFromVec (tName c) c) x)

instance GPU (Vertex Float) where
    type CPU (Vertex Float) = Float
    toGPU = Vertex . ShaderUniform . UniformFloat
instance GPU (Vertex Int) where
    type CPU (Vertex Int) = Int
    toGPU = Vertex . ShaderUniform . UniformInt
instance GPU (Vertex Bool) where
    type CPU (Vertex Bool) = Bool
    toGPU = Vertex . ShaderUniform . UniformBool

instance GPU (Fragment Float) where
    type CPU (Fragment Float) = Float
    toGPU = Fragment . ShaderUniform . UniformFloat
instance GPU (Fragment Int) where
    type CPU (Fragment Int) = Int
    toGPU = Fragment . ShaderUniform . UniformInt
instance GPU (Fragment Bool) where
    type CPU (Fragment Bool) = Bool
    toGPU = Fragment . ShaderUniform . UniformBool

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

instance Eq (Vertex a) where
  (==) = noFun "(==)"
  (/=) = noFun "(/=)" 
instance Eq (Fragment a) where
  (==) = noFun "(==)"
  (/=) = noFun "(/=)"
instance Show (Vertex a) where
  show      = noFun "show"
instance Show (Fragment a) where
  show      = noFun "show"

instance Ord (Vertex Float) where
  (<=) = noFun "(<=)"
  min = vBinaryFunc "float" "min"
  max = vBinaryFunc "float" "max"
instance Ord (Fragment Float) where
  (<=) = noFun "(<=)"
  min = fBinaryFunc "float" "min"
  max = fBinaryFunc "float" "max"
instance Num (Vertex Float) where
  negate      = vUnaryPreOp "float" "-"
  (+)         = vBinaryOp "float" "+"
  (*)         = vBinaryOp "float" "*"
  fromInteger = Vertex . ShaderConstant . ConstFloat . fromInteger
  abs         = vUnaryFunc "float" "abs"
  signum      = vUnaryFunc "float" "sign"
instance Num (Fragment Float) where
  negate      = fUnaryPreOp "float" "-"
  (+)         = fBinaryOp "float" "+"
  (*)         = fBinaryOp "float" "*"
  fromInteger = Fragment . ShaderConstant . ConstFloat . fromInteger
  abs         = fUnaryFunc "float" "abs"
  signum      = fUnaryFunc "float" "sign"
  

instance Ord (Vertex Int) where
  (<=) = noFun "(<=)"
  min = noFun "min"
  max = noFun "max"
instance Ord (Fragment Int) where
  (<=) = noFun "(<=)"
  min = noFun "min"
  max = noFun "max"
instance Num (Vertex Int) where
  negate      = vUnaryPreOp "int" "-"
  (+)         = vBinaryOp "int" "+"
  (*)         = vBinaryOp "int" "*"
  fromInteger = Vertex . ShaderConstant . ConstInt . fromInteger
  abs         = noFun "abs"
  signum      = noFun "sign"
instance Num (Fragment Int) where
  negate      = fUnaryPreOp "int" "-"
  (+)         = fBinaryOp "int" "+"
  (*)         = fBinaryOp "int" "*"
  fromInteger = Fragment . ShaderConstant . ConstInt . fromInteger
  abs         = noFun "abs"
  signum      = noFun "sign"
  
    
instance Fractional (Vertex Float) where
  (/)          = vBinaryOp "float" "/"
  fromRational = Vertex . ShaderConstant . ConstFloat . fromRational
instance  Fractional (Fragment Float) where
  (/)          = fBinaryOp "float" "/"
  fromRational = Fragment . ShaderConstant . ConstFloat . fromRational
instance Floating (Vertex Float) where
  pi    = Vertex $ ShaderConstant $ ConstFloat pi
  sqrt  = vUnaryFunc "float" "sqrt"
  exp   = vUnaryFunc "float" "exp"
  log   = vUnaryFunc "float" "log"
  (**)  = vBinaryFunc "float" "pow"
  sin   = vUnaryFunc "float" "sin"
  cos   = vUnaryFunc "float" "cos"
  tan   = vUnaryFunc "float" "tan"
  asin  = vUnaryFunc "float" "asin"
  acos  = vUnaryFunc "float" "acos"
  atan  = vUnaryFunc "float" "atan"
  sinh  = noFun "float" "sinh"
  cosh  = noFun "float" "cosh"
  asinh = noFun "float" "asinh"
  atanh = noFun "float" "atanh"
  acosh = noFun "float" "acosh"
instance Floating (Fragment Float) where
  pi    = Fragment $ ShaderConstant $ ConstFloat pi
  sqrt  = fUnaryFunc "float" "sqrt"
  exp   = fUnaryFunc "float" "exp"
  log   = fUnaryFunc "float" "log"
  (**)  = fBinaryFunc "float" "pow"
  sin   = fUnaryFunc "float" "sin"
  cos   = fUnaryFunc "float" "cos"
  tan   = fUnaryFunc "float" "tan"
  asin  = fUnaryFunc "float" "asin"
  acos  = fUnaryFunc "float" "acos"
  atan  = fUnaryFunc "float" "atan"
  sinh  = noFun "sinh"
  cosh  = noFun "cosh"
  asinh = noFun "asinh"
  atanh = noFun "atanh"
  acosh = noFun "acosh"
 
-- | This class provides the GPU functions either not found in Prelude's numerical classes, or that has wrong types.
--   Instances are also provided for normal 'Float's and 'Double's.
--   Minimal complete definition: 'floor'' and 'ceiling''.
class (Ord a, Floating a) => Real' a where
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
  clamp x a = min (max x a)
  saturate x = clamp x 0 1
  mix x y a = x*(1-a)+y*a
  step a x | x < a     = 0
           | otherwise = 1
  smoothstep a b x = let t = saturate ((x-a) / (b-a))
                     in t*t*(3-2*t)
  fract' x = x - floor' x
  mod' x y = x - y* floor' (x/y)
  
instance Real' Float where
  floor' = fromIntegral . floor
  ceiling' = fromIntegral . ceiling

instance Real' Double where
  floor' = fromIntegral . floor
  ceiling' = fromIntegral . ceiling
  
instance Real' (Vertex Float) where
  rsqrt = vUnaryFunc "float" "inversesqrt"
  exp2 = vUnaryFunc "float" "exp2"
  log2 = vUnaryFunc "float" "log2"
  floor' = vUnaryFunc "float" "floor"
  ceiling' = vUnaryFunc "float" "ceil"
  fract' = vUnaryFunc "float" "fract"
  mod' = vBinaryFunc "float" "mod"
  clamp = vTernaryFunc "float" "clamp"
  mix = vTernaryFunc "float" "mix"
  step = vBinaryFunc "float" "step"
  smoothstep = vTernaryFunc "float" "smoothstep"
  
instance Real' (Fragment Float) where
  rsqrt = fUnaryFunc "float" "inversesqrt"
  exp2 = fUnaryFunc "float" "exp2"
  log2 = fUnaryFunc "float" "log2"
  floor' = fUnaryFunc "float" "floor"
  ceiling' = fUnaryFunc "float" "ceil"
  fract' = fUnaryFunc "float" "fract"
  mod' = fBinaryFunc "float" "mod"
  clamp = fTernaryFunc "float" "clamp"
  mix = fTernaryFunc  "float" "mix"
  step = fBinaryFunc "float" "step"
  smoothstep = fTernaryFunc "float" "smoothstep"

instance Boolean (Vertex Bool) where
    true = Vertex $ ShaderConstant $ ConstBool True
    false = Vertex $ ShaderConstant $ ConstBool False
    notB = vUnaryPreOp "bool" "!"
    (&&*) = vBinaryOp "bool" "&&"
    (||*) = vBinaryOp "bool" "||"
instance Boolean (Fragment Bool) where
    true = Fragment $ ShaderConstant $ ConstBool True
    false = Fragment $ ShaderConstant $ ConstBool False
    notB = fUnaryPreOp "bool" "!"
    (&&*) = fBinaryOp "bool" "&&"
    (||*) = fBinaryOp "bool" "||"
instance Eq a => EqB (Vertex Bool) (Vertex a) where
    (==*) = vBinaryOp "bool" "=="
    (/=*) = vBinaryOp "bool" "!="
instance Eq a => EqB (Fragment Bool) (Fragment a) where
    (==*) = fBinaryOp "bool" "=="
    (/=*) = fBinaryOp "bool" "!="
instance Ord a => OrdB (Vertex Bool) (Vertex a) where
    (<*) = vBinaryOp "bool" "<"
    (>=*) = vBinaryOp "bool" ">="
    (>*) = vBinaryOp "bool" ">"
    (<=*) = vBinaryOp "bool" "<="
instance Ord a => OrdB (Fragment Bool) (Fragment a) where
    (<*) = fBinaryOp "bool" "<"
    (>=*) = fBinaryOp "bool" ">="
    (>*) = fBinaryOp "bool" ">"
    (<=*) = fBinaryOp "bool" "<="

instance IfB (Vertex Bool) (Vertex Int) where
    ifB c a b = Vertex $ ShaderOp "if" (assign "int" (\[a,b,c]->a++"?"++b++":"++c)) [fromVertex c,fromVertex a,fromVertex b]
instance IfB (Vertex Bool) (Vertex Float) where
    ifB c a b = Vertex $ ShaderOp "if" (assign "float" (\[a,b,c]->a++"?"++b++":"++c)) [fromVertex c,fromVertex a,fromVertex b]
instance IfB (Vertex Bool) (Vertex Bool) where
    ifB c a b = Vertex $ ShaderOp "if" (assign "bool" (\[a,b,c]->a++"?"++b++":"++c)) [fromVertex c,fromVertex a,fromVertex b]
    
instance IfB (Fragment Bool) (Fragment Int) where
    ifB c a b = Fragment $ ShaderOp "if" (assign "int" (\[a,b,c]->a++"?"++b++":"++c)) [fromFragment c,fromFragment a,fromFragment b]
instance IfB (Fragment Bool) (Fragment Float) where
    ifB c a b = Fragment $ ShaderOp "if" (assign "float" (\[a,b,c]->a++"?"++b++":"++c)) [fromFragment c,fromFragment a,fromFragment b]
instance IfB (Fragment Bool) (Fragment Bool) where
    ifB c a b = Fragment $ ShaderOp "if" (assign "bool" (\[a,b,c]->a++"?"++b++":"++c)) [fromFragment c,fromFragment a,fromFragment b]

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
instance Convert (Vertex Float) where
    type ConvertFloat (Vertex Float) = Vertex Float
    type ConvertInt (Vertex Float) = Vertex Int
    toFloat = id
    toInt = vUnaryFunc "int" "int"
instance Convert (Vertex Int) where
    type ConvertFloat (Vertex Int) = Vertex Float
    type ConvertInt (Vertex Int) = Vertex Int
    toFloat = vUnaryFunc "float" "float"
    toInt = id
instance Convert (Fragment Float) where
    type ConvertFloat (Fragment Float) = Fragment Float
    type ConvertInt (Fragment Float) = Fragment Int
    toFloat = id
    toInt = fUnaryFunc "int" "int"
instance Convert (Fragment Int) where
    type ConvertFloat (Fragment Int) = Fragment Float
    type ConvertInt (Fragment Int) = Fragment Int
    toFloat = fUnaryFunc "float" "float"
    toInt = id
    
-- | The derivative in x using local differencing of the rasterized value.
dFdx :: Fragment Float -> Fragment Float
-- | The derivative in y using local differencing of the rasterized value.
dFdy :: Fragment Float -> Fragment Float
-- | The sum of the absolute derivative in x and y using local differencing of the rasterized value.
fwidth :: Fragment Float -> Fragment Float
dFdx = fUnaryFunc "float" "dFdx"
dFdy = fUnaryFunc "float" "dFdy"
fwidth = fUnaryFunc "float" "fwidth"

--------------------------------------
-- Vector specializations

{-# RULES "norm/F4" norm = normF4 #-}
{-# RULES "norm/F3" norm = normF3 #-}
{-# RULES "norm/F2" norm = normF2 #-}
normF4 :: Vec4 (Fragment Float) -> Fragment Float
normF4 = fUnaryFunc "float" "length" . fFromVec "vec4"
normF3 :: Vec3 (Fragment Float) -> Fragment Float
normF3 = fUnaryFunc "float" "length" . fFromVec "vec3"
normF2 :: Vec2 (Fragment Float) -> Fragment Float
normF2 = fUnaryFunc "float" "length" . fFromVec "vec2"
{-# RULES "norm/V4" norm = normV4 #-}
{-# RULES "norm/V3" norm = normV3 #-}
{-# RULES "norm/V2" norm = normV2 #-}
normV4 :: Vec4 (Vertex Float) -> Vertex Float
normV4 = vUnaryFunc "float" "length" . vFromVec "vec4"
normV3 :: Vec3 (Vertex Float) -> Vertex Float
normV3 = vUnaryFunc "float" "length" . vFromVec "vec3"
normV2 :: Vec2 (Vertex Float) -> Vertex Float
normV2 = vUnaryFunc "float" "length" . vFromVec "vec3"

{-# RULES "normalize/F4" normalize = normalizeF4 #-}
{-# RULES "normalize/F3" normalize = normalizeF3 #-}
{-# RULES "normalize/F2" normalize = normalizeF2 #-}
normalizeF4 :: Vec4 (Fragment Float) -> Vec4 (Fragment Float)
normalizeF4 = fToVec "float" 4 . fUnaryFunc "vec4" "normalize" . fFromVec "vec4"
normalizeF3 :: Vec3 (Fragment Float) -> Vec3 (Fragment Float)
normalizeF3 = fToVec "float" 3 . fUnaryFunc "vec3" "normalize" . fFromVec "vec3"
normalizeF2 :: Vec2 (Fragment Float) -> Vec2 (Fragment Float)
normalizeF2 = fToVec "float" 2 . fUnaryFunc "vec2" "normalize" . fFromVec "vec2"
{-# RULES "normalize/V4" normalize = normalizeV4 #-}
{-# RULES "normalize/V3" normalize = normalizeV3 #-}
{-# RULES "normalize/V2" normalize = normalizeV2 #-}
normalizeV4 :: Vec4 (Vertex Float) -> Vec4 (Vertex Float)
normalizeV4 = vToVec "float" 4 . vUnaryFunc "vec4" "normalize" . vFromVec "vec4"
normalizeV3 :: Vec3 (Vertex Float) -> Vec3 (Vertex Float)
normalizeV3 = vToVec "float" 3 . vUnaryFunc "vec3" "normalize" . vFromVec "vec3"
normalizeV2 :: Vec2 (Vertex Float) -> Vec2 (Vertex Float)
normalizeV2 = vToVec "float" 2 . vUnaryFunc "vec2" "normalize" . vFromVec "vec2"

{-# RULES "dot/F4" dot = dotF4 #-}
{-# RULES "dot/F3" dot = dotF3 #-}
{-# RULES "dot/F2" dot = dotF2 #-}
dotF4 :: Vec4 (Fragment Float) -> Vec4 (Fragment Float) -> Fragment Float
dotF4 a b = fBinaryFunc "float" "dot" (fFromVec "vec4" a) (fFromVec "vec4" b)
dotF3 :: Vec3 (Fragment Float) -> Vec3 (Fragment Float) -> Fragment Float
dotF3 a b = fBinaryFunc "float" "dot" (fFromVec "vec3" a) (fFromVec "vec3" b)
dotF2 :: Vec2 (Fragment Float) -> Vec2 (Fragment Float) -> Fragment Float
dotF2 a b = fBinaryFunc "float" "dot" (fFromVec "vec2" a) (fFromVec "vec2" b)
{-# RULES "dot/V4" dot = dotV4 #-}
{-# RULES "dot/V3" dot = dotV3 #-}
{-# RULES "dot/V2" dot = dotV2 #-}
dotV4 :: Vec4 (Vertex Float) -> Vec4 (Vertex Float) -> Vertex Float
dotV4 a b = vBinaryFunc "float" "dot" (vFromVec "vec4" a) (vFromVec "vec4" b)
dotV3 :: Vec3 (Vertex Float) -> Vec3 (Vertex Float) -> Vertex Float
dotV3 a b = vBinaryFunc "float" "dot" (vFromVec "vec3" a) (vFromVec "vec3" b)
dotV2 :: Vec2 (Vertex Float) -> Vec2 (Vertex Float) -> Vertex Float
dotV2 a b = vBinaryFunc "float" "dot" (vFromVec "vec2" a) (vFromVec "vec2" b)

{-# RULES "cross/F3" cross = crossF3 #-}
crossF3 :: Vec3 (Fragment Float) -> Vec3 (Fragment Float) -> Vec3 (Fragment Float)
crossF3 a b = fToVec "float" 3 $ fBinaryFunc "vec3" "cross" (fFromVec "vec3" a) (fFromVec "vec3" b)
{-# RULES "cross/V3" cross = crossV3 #-}
crossV3 :: Vec3 (Vertex Float) -> Vec3 (Vertex Float) ->Vec3 (Vertex Float)
crossV3 a b = vToVec "float" 3 $ vBinaryFunc "vec3" "cross" (vFromVec "vec3" a) (vFromVec "vec3" b)

--------------------------------------
-- Private
--
noFun :: String -> a
noFun = error . (++ ": No overloading for Vertex/Fragment")

setVaryings xs = setVaryings' 0 $ map (('t':) . show) xs
    where 
        setVaryings' _ [] = ""
        setVaryings' n xs = case splitAt 4 xs of (ys,rest) -> "f" ++ show n ++ " = " ++ tName' (length ys) ++ "(" ++ intercalate "," ys ++ ");\n" ++ setVaryings' (n+1) rest

inoutDecls t n i = inoutDecls' i 0 
    where inoutDecls' i x | i >= 4    = t ++ " vec4 " ++ n ++ show x ++ ";\n" ++ inoutDecls' (i-4) (x+1)
                          | i == 0    = ""
                          | otherwise = t ++ " " ++ tName' i ++ " " ++ n ++ show x ++ ";\n"
          
uniformDecls :: String -> UniformSet -> String
uniformDecls p (f,i,b,s) = makeU "float" "f" (length f) ++
                           makeU "int" "i" (length i) ++
                           makeU "bool" "b" (length b) ++
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
    where startDAG xs = do ht <- HT.new (==) (fromIntegral . hashStableName)
                           runStateT (mapM (createDAG' ht) xs) []
          createDAG' :: HT.HashTable (StableName ShaderTree) Int -> ShaderTree -> StateT [(ShaderTree, [Int])] IO Int
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
    where getCode ((f,i,b,s),inlns) (n, (ShaderUniform (UniformFloat _), _)) = (((f+1,i,b,s),inlns), assign "float" (const $ inName ++ "uf[" ++ show f ++ "]") (var n) [])
          getCode ((f,i,b,s),inlns) (n, (ShaderUniform (UniformInt _), _)) = (((f,i+1,b,s),inlns), assign "int" (const $ inName ++ "ui[" ++ show i ++ "]") (var n) [])
          getCode ((f,i,b,s),inlns) (n, (ShaderUniform (UniformBool _), _)) = (((f,i,b+1,s),inlns), assign "bool" (const $ inName ++ "ub[" ++ show b ++ "]") (var n) [])
          getCode ((f,i,b,s),inlns) (n, (ShaderUniform (UniformSampler t _ _), _)) =
                case first (fromMaybe 0) $ Map.insertLookupWithKey (const $ const (+1)) t 1 s of
                    (x, s') -> (((f,i,b,s'),Map.insert n (inName ++ "us" ++ show (fromEnum t) ++ "[" ++ show x ++ "]") inlns), "") 
          getCode x (n, (ShaderConstant (ConstFloat f), _)) = (x, assign "float" (const $ show f) (var n) [])
          getCode x (n, (ShaderConstant (ConstInt i), _)) = (x, assign "int" (const $ show i) (var n) [])
          getCode x (n, (ShaderConstant (ConstBool b), _)) = (x, assign "bool" (const $ if b then "true" else "false") (var n) [])
          getCode x (n, (ShaderInput i, _)) = (x, assign "float" (const $ inName ++ inoutAccessor (inF i) numIns) (var n) [])
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
tName' 1 = "float"
tName' x = "vec" ++ show x

assign :: String -> ([String] -> String) -> String -> [String] -> String
assign t f x ys = t ++ " " ++ x ++ "=" ++ f ys ++ ";\n"
binFunc :: String -> [String] -> String
binFunc s = head . binFunc'
    where
        binFunc' (a:b:xs) = binFunc' $ (s ++ "(" ++ a ++ "," ++ b ++ ")"):binFunc' xs
        binFunc' x = x

vBinaryOp t s a b = Vertex $ ShaderOp s (assign t (intercalate s)) [fromVertex a, fromVertex b]
vUnaryPreOp t s a = Vertex $ ShaderOp s (assign t ((s ++) . head)) [fromVertex a]
vUnaryPostOp t s a = Vertex $ ShaderOp s (assign t ((++ s) . head)) [fromVertex a]
vUnaryFunc t s a = Vertex $ ShaderOp s (assign t (((s ++ "(") ++) . (++ ")") . head)) [fromVertex a]
vBinaryFunc t s a b = Vertex $ ShaderOp s (assign t (binFunc s)) [fromVertex a, fromVertex b]
vTernaryFunc t s a b c = Vertex $ ShaderOp s (assign t (\[a,b,c]->s++"("++a++","++b++","++c++")")) [fromVertex a, fromVertex b, fromVertex c]
vFromVec t = Vertex . ShaderOp "" (assign t (((t ++ "(") ++) . (++ ")") . intercalate ",")) . map fromVertex . Vec.toList 
vToVec t n a = Vec.fromList $ map (\s -> Vertex $ ShaderOp s (assign t (\[x]->x++"["++s++"]")) [fromVertex a]) [show n' | n' <-[0..n - 1]]

fBinaryOp t s a b = Fragment $ ShaderOp s (assign t (intercalate s)) [fromFragment a, fromFragment b]
fUnaryPreOp t s a = Fragment $ ShaderOp s (assign t ((s ++) . head)) [fromFragment a]
fUnaryPostOp t s a = Fragment $ ShaderOp s (assign t ((++ s) . head)) [fromFragment a]
fUnaryFunc t s a = Fragment $ ShaderOp s (assign t (((s ++ "(") ++) . (++ ")") . head)) [fromFragment a]
fBinaryFunc t s a b = Fragment $ ShaderOp s (assign t (binFunc s)) [fromFragment a, fromFragment b]
fTernaryFunc t s a b c = Fragment $ ShaderOp s (assign t (\[a,b,c]->s++"("++a++","++b++","++c++")")) [fromFragment a, fromFragment b, fromFragment c]
fFromVec t = Fragment . ShaderOp "" (assign t (((t ++ "(") ++) . (++ ")") . intercalate ",")) . map fromFragment . Vec.toList 
fToVec t n a = Vec.fromList $ map (\s -> Fragment $ ShaderOp s (assign t (\[x]->x++"["++s++"]")) [fromFragment a]) [show n' | n' <-[0..n - 1]]

