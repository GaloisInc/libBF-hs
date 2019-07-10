module LibBF
  ( BigFloat
  , bfPosZero, bfNegZero
  , bfPosInf, bfNegInf
  , bfNaN

  , bfFromWord
  , bfFromInt
  , bfFromDouble

  , bfIsFinite
  , bfIsZero
  , bfIsNaN

  , bfAdd, bfSub, bfMul, bfDiv, bfMod, bfRem
  , bfSqrt
  , bfPowWord

  , bfRoundFloat, bfRoundInt

  , bfToDouble
  , bfToString

  , module LibBF.Opts
  ) where


import Data.Word
import Data.Int
import System.IO.Unsafe

import LibBF.Mutable as M
import LibBF.Opts



newtype BigFloat = BigFloat BF

instance Show BigFloat where
  show = bfToString 10 (showFixed 25)

{-# NOINLINE ctxt #-}
{-# OPTIONS_GHC -fno-cse #-}
ctxt :: BFContext
ctxt = unsafePerformIO newContext

newBigFloat :: (BF -> IO ()) -> BigFloat
newBigFloat f = unsafe $
  do bf <- new ctxt
     f bf
     pure (BigFloat bf)

newBigFloat' :: (BF -> IO a) -> (BigFloat,a)
newBigFloat' f = unsafe $
  do bf <- new ctxt
     a <- f bf
     pure (BigFloat bf, a)



unsafe :: IO a -> a
unsafe = unsafeDupablePerformIO

--------------------------------------------------------------------------------
-- Constants

bfPosZero :: BigFloat
bfPosZero = newBigFloat (setZero Pos)

bfNegZero :: BigFloat
bfNegZero = newBigFloat (setZero Neg)

bfPosInf :: BigFloat
bfPosInf = newBigFloat (setInf Pos)

bfNegInf :: BigFloat
bfNegInf = newBigFloat (setInf Neg)

bfNaN :: BigFloat
bfNaN = newBigFloat setNaN

bfFromWord :: Word64 -> BigFloat
bfFromWord = newBigFloat . setWord

bfFromInt :: Int64 -> BigFloat
bfFromInt = newBigFloat . setInt

bfFromDouble :: Double -> BigFloat
bfFromDouble = newBigFloat . setDouble

instance Eq BigFloat where
  BigFloat x == BigFloat y = unsafe (cmpEq x y)

instance Ord BigFloat where
  compare (BigFloat x) (BigFloat y) = unsafe (cmp x y)
  BigFloat x < BigFloat y           = unsafe (cmpLT x y)
  BigFloat x <= BigFloat y          = unsafe (cmpLEQ x y)

bfIsFinite :: BigFloat -> Bool
bfIsFinite (BigFloat x) = unsafe (isFinite x)

bfIsNaN :: BigFloat -> Bool
bfIsNaN (BigFloat x) = unsafe (M.isNaN x)

bfIsZero :: BigFloat -> Bool
bfIsZero (BigFloat x) = unsafe (isZero x)

bfAdd :: BFOpts -> BigFloat -> BigFloat -> (BigFloat,Status)
bfAdd opt (BigFloat x) (BigFloat y) = newBigFloat' (fadd opt x y)

bfSub :: BFOpts -> BigFloat -> BigFloat -> (BigFloat,Status)
bfSub opt (BigFloat x) (BigFloat y) = newBigFloat' (fsub opt x y)

bfMul :: BFOpts -> BigFloat -> BigFloat -> (BigFloat,Status)
bfMul opt (BigFloat x) (BigFloat y) = newBigFloat' (fmul opt x y)

bfDiv :: BFOpts -> BigFloat -> BigFloat -> (BigFloat,Status)
bfDiv opt (BigFloat x) (BigFloat y) = newBigFloat' (fdiv opt x y)

bfMod :: BFOpts -> BigFloat -> BigFloat -> (BigFloat,Status)
bfMod opt (BigFloat x) (BigFloat y) = newBigFloat' (fmod opt x y)

bfRem :: BFOpts -> BigFloat -> BigFloat -> (BigFloat,Status)
bfRem opt (BigFloat x) (BigFloat y) = newBigFloat' (frem opt x y)

bfSqrt :: BFOpts -> BigFloat -> (BigFloat,Status)
bfSqrt opt (BigFloat x) = newBigFloat' (fsqrt opt x)

-- | Round to a float matching the input parameters.
bfRoundFloat :: BFOpts -> BigFloat -> (BigFloat,Status)
bfRoundFloat opt (BigFloat x) = newBigFloat' (\bf ->
  do setBF x bf
     fround opt bf
  )

-- | Round to an integer using the given parameters.
bfRoundInt :: BFOpts -> BigFloat -> (BigFloat,Status)
bfRoundInt opt (BigFloat x) = newBigFloat' (\bf ->
  do setBF x bf
     frint opt bf
  )

bfPowWord :: BFOpts -> BigFloat -> Word64 -> (BigFloat, Status)
bfPowWord opt (BigFloat x) w = newBigFloat' (fpowWord opt x w)

bfToDouble :: RoundMode -> BigFloat -> (Double, Status)
bfToDouble r (BigFloat x) = unsafe (toDouble r x)

bfToString :: Int -> ShowFmt -> BigFloat -> String
bfToString radix opts (BigFloat x) =
  unsafe (toString radix opts x)

--------------------------------------------------------------------------------




