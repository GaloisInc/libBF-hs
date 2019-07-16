{-# Language Trustworthy #-}
module LibBF
  (
    -- * Constants
    BigFloat
  , bfPosZero, bfNegZero
  , bfPosInf, bfNegInf
  , bfNaN

    -- * Conversions
  , bfFromWord
  , bfFromInt
  , bfFromDouble
  , bfFromInteger
  , bfToDouble
  , bfToString
  , bfToRep

    -- * Predicates
  , bfIsFinite
  , bfIsZero
  , bfIsNaN
  , bfCompare
  , bfSign
  , bfExponent
  , Sign(..)

    -- * Arithmetic
  , bfNeg
  , bfAdd, bfSub, bfMul, bfDiv, bfMod, bfRem
  , bfMulWord, bfMulInt, bfMul2Exp
  , bfSqrt
  , bfPow, bfPowWord, bfPowWordWord

    -- * Rounding
  , bfRoundFloat, bfRoundInt

    -- * Mutability
  , bfUnsafeThaw
  , bfUnsafeFreeze

    -- * Limits


    -- * Configuration
  , module LibBF.Opts
  ) where


import Data.Word
import Data.Int
import System.IO.Unsafe

import LibBF.Mutable as M
import LibBF.Opts
import Control.DeepSeq


-- | Arbitrary precision floating point numbers.
newtype BigFloat = BigFloat BF

instance NFData BigFloat where
  rnf x = x `seq` ()


instance Show BigFloat where
  show = bfToString 16 (showFreeMin Nothing <> addPrefix)

--------------------------------------------------------------------------------
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
unsafe = unsafePerformIO

--------------------------------------------------------------------------------
-- Constants

-- | Positive zero.
bfPosZero :: BigFloat
bfPosZero = newBigFloat (setZero Pos)

-- | Negative zero.
bfNegZero :: BigFloat
bfNegZero = newBigFloat (setZero Neg)

-- | Positive infinity.
bfPosInf :: BigFloat
bfPosInf = newBigFloat (setInf Pos)

-- | Negative infinity.
bfNegInf :: BigFloat
bfNegInf = newBigFloat (setInf Neg)

-- | Not-a-number.
bfNaN :: BigFloat
bfNaN = newBigFloat setNaN

-- | A floating point number corresponding to the given word.
bfFromWord :: Word64 -> BigFloat
bfFromWord = newBigFloat . setWord

-- | A floating point number corresponding to the given int.
bfFromInt :: Int64 -> BigFloat
bfFromInt = newBigFloat . setInt

-- | A floating point number corresponding to the given double.
bfFromDouble :: Double -> BigFloat
bfFromDouble = newBigFloat . setDouble

bfFromInteger :: Integer -> BigFloat
bfFromInteger = newBigFloat . setInteger

-- | IEEE 754 equality
instance Eq BigFloat where
  BigFloat x == BigFloat y = unsafe (cmpEq x y)

-- | IEEE 754 comparisons
instance Ord BigFloat where
  BigFloat x < BigFloat y  = unsafe (cmpLT x y)
  BigFloat x <= BigFloat y = unsafe (cmpLEQ x y)


{-| Compare the two numbers.  The special values are ordered like this:

      * -0 < 0
      * NaN == NaN
      * NaN is larger than all other numbers

Note that these differ from 'bfIsLT' and 'bfIsLEQ'.
-}
bfCompare :: BigFloat -> BigFloat -> Ordering
bfCompare (BigFloat x) (BigFloat y) = unsafe (cmp x y)


-- | Is this a "normal" (i.e., non-infinite, non NaN) number.
bfIsFinite :: BigFloat -> Bool
bfIsFinite (BigFloat x) = unsafe (isFinite x)

-- | Is this value NaN.
bfIsNaN :: BigFloat -> Bool
bfIsNaN (BigFloat x) = unsafe (M.isNaN x)

-- | Get the sign of a number.  Assumes the input is not NaN.
bfSign :: BigFloat -> Maybe Sign
bfSign (BigFloat x) = unsafe (getSign x)

-- | Get the exponent for the given number.
-- Infinity, zero and NaN do not have an exponent.
bfExponent :: BigFloat -> Maybe Int64
bfExponent (BigFloat x) = unsafe (getExp x)

-- | Is this value a zero.
bfIsZero :: BigFloat -> Bool
bfIsZero (BigFloat x) = unsafe (isZero x)

bfNeg :: BigFloat -> BigFloat
bfNeg (BigFloat x) = newBigFloat (\bf -> setBF x bf >> fneg bf)

-- | Add two numbers useing the given options.
bfAdd :: BFOpts -> BigFloat -> BigFloat -> (BigFloat,Status)
bfAdd opt (BigFloat x) (BigFloat y) = newBigFloat' (fadd opt x y)

-- | Subtract two numbers useing the given options.
bfSub :: BFOpts -> BigFloat -> BigFloat -> (BigFloat,Status)
bfSub opt (BigFloat x) (BigFloat y) = newBigFloat' (fsub opt x y)

-- | Multiply two numbers using the given options.
bfMul :: BFOpts -> BigFloat -> BigFloat -> (BigFloat,Status)
bfMul opt (BigFloat x) (BigFloat y) = newBigFloat' (fmul opt x y)

-- | Multiply a number and a word, using the given options.
bfMulWord :: BFOpts -> BigFloat -> Word64 -> (BigFloat,Status)
bfMulWord opt (BigFloat x) y = newBigFloat' (fmulWord opt x y)

-- | Multiply a number and an int, using the given options.
bfMulInt :: BFOpts -> BigFloat -> Int64 -> (BigFloat,Status)
bfMulInt opt (BigFloat x) y = newBigFloat' (fmulInt opt x y)

-- | Multiply a number by @2^e@.
bfMul2Exp :: BFOpts -> BigFloat -> Int64 -> (BigFloat,Status)
bfMul2Exp opt (BigFloat x) e = newBigFloat' (\p ->
  do setBF x p
     fmul2Exp opt e p)

-- | Divide two numbers useing the given options.
bfDiv :: BFOpts -> BigFloat -> BigFloat -> (BigFloat,Status)
bfDiv opt (BigFloat x) (BigFloat y) = newBigFloat' (fdiv opt x y)

-- | Modulo of two numbers useing the given options.
bfMod :: BFOpts -> BigFloat -> BigFloat -> (BigFloat,Status)
bfMod opt (BigFloat x) (BigFloat y) = newBigFloat' (fmod opt x y)

-- | Reminder of two numbers useing the given options.
bfRem :: BFOpts -> BigFloat -> BigFloat -> (BigFloat,Status)
bfRem opt (BigFloat x) (BigFloat y) = newBigFloat' (frem opt x y)

-- | Square root of two numbers useing the given options.
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

-- | Exponentiate a float to a positive integer power.
bfPowWord :: BFOpts -> BigFloat -> Word64 -> (BigFloat, Status)
bfPowWord opt (BigFloat x) w = newBigFloat' (fpowWord opt x w)

-- | Exponentiate a word to a positive integer power.
bfPowWordWord :: BFOpts -> Word64 -> Word64 -> (BigFloat, Status)
bfPowWordWord opts x y = newBigFloat' (fpowWordWord opts x y)

-- | Exponentiate a word to a positive integer power.
bfPow :: BFOpts -> BigFloat -> BigFloat -> (BigFloat, Status)
bfPow opts (BigFloat x) (BigFloat y) = newBigFloat' (fpow opts x y)

-- | Constant to a 'Double'
bfToDouble :: RoundMode -> BigFloat -> (Double, Status)
bfToDouble r (BigFloat x) = unsafe (toDouble r x)

-- | Render as a 'String', using the given settings.
bfToString :: Int {- ^ Base -} -> ShowFmt -> BigFloat -> String
bfToString radix opts (BigFloat x) =
  unsafe (toString radix opts x)

-- | The float as an exponentiated 'Integer'.
bfToRep :: BigFloat -> BFRep
bfToRep (BigFloat x) = unsafe (toRep x)

-- | Make a number mutable.
-- WARNING: This does not copy the number,
-- so it could break referential transperancy.
bfUnsafeThaw :: BigFloat -> BF
bfUnsafeThaw (BigFloat x) = x

-- | Make a number immutable.
-- WARNING: This does not copy the number,
-- so it could break referential transperancy.
bfUnsafeFreeze :: BF -> BigFloat
bfUnsafeFreeze = BigFloat

--------------------------------------------------------------------------------




