{-# Language ForeignFunctionInterface, CApiFFI #-}
{-# Language PatternSynonyms #-}
module LibBF.Mutable
  ( -- * Allocation
    newContext, BFContext
  , new, BF

    -- * Assignment
  , setNaN
  , setZero
  , setInf
  , Sign(..)
  , setWord
  , setInt
  , setDouble
  , setBF

    -- * Queries and Comparisons
  , cmpEq
  , cmpLT
  , cmpLEQ
  , cmpAbs
  , cmp

  , isFinite
  , LibBF.Mutable.isNaN
  , isZero

    -- * Arithmetic
  , fneg
  , fadd
  , fsub
  , fmul
  , fdiv
  , fmod
  , frem
  , fsqrt
  , fpowWord
  , fround
  , frint

  -- * Convert from a number
  , toDouble
  , toString

  -- * Configuration
  , module LibBF.Opts
  ) where


import Foreign.Storable(peek)
import Foreign.Marshal.Alloc(alloca,free)
import Foreign.Ptr(Ptr,FunPtr)
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String
import Data.Word
import Data.Int

#include <libbf.h>

import LibBF.Opts

-- | State of the current computation context.
newtype BFContext = BFContext (ForeignPtr BFContext)

foreign import ccall "bf_context_init_hs"
  bf_context_init_hs :: Ptr BFContext -> IO ()

foreign import ccall "&bf_context_end"
  bf_context_end :: FunPtr (Ptr BFContext -> IO ())

{-| Allocate a new numeric context. -}
newContext :: IO BFContext
newContext =
  do fptr <- mallocForeignPtrBytes #{size bf_context_t}
     withForeignPtr fptr bf_context_init_hs
     addForeignPtrFinalizer bf_context_end fptr
     pure (BFContext fptr)


-- | A mutable high precision floating point number.
newtype BF = BF (ForeignPtr BF)

foreign import ccall "bf_init"
  bf_init :: Ptr BFContext -> Ptr BF -> IO ()

foreign import ccall "&bf_delete_hs"
  bf_delete :: FunPtr (Ptr BF -> IO ())

{-| Allocate a new number.  Starts off as zero. -}
new :: BFContext -> IO BF
new (BFContext fctx) =
  withForeignPtr fctx (\ctx ->
  do fptr <- mallocForeignPtrBytes #{size bf_t}
     withForeignPtr fptr (bf_init ctx)
     addForeignPtrFinalizer bf_delete fptr
     pure (BF fptr)
  )

--------------------------------------------------------------------------------
-- FFI Helpers

signToC :: Sign -> CInt
signToC s = case s of
              Pos -> 0
              Neg -> 1

asBool :: CInt -> Bool
asBool = (/= 0)

asOrd :: CInt -> Ordering
asOrd x
  | x < 0     = LT
  | x > 0     = GT
  | otherwise = EQ


bf1 :: (Ptr BF -> IO a) -> BF -> IO a
bf1 f (BF fptr) = withForeignPtr fptr f

bfQuery :: (Ptr BF -> IO CInt) -> BF -> IO Bool
bfQuery f = bf1 (fmap asBool . f)

bfRel :: (Ptr BF -> Ptr BF -> IO CInt) -> BF -> BF -> IO Bool
bfRel f = bf2 (\x y -> asBool <$> f x y)

bfOrd :: (Ptr BF -> Ptr BF -> IO CInt) -> BF -> BF -> IO Ordering
bfOrd f = bf2 (\x y -> asOrd <$> f x y)

bf2 :: (Ptr BF -> Ptr BF -> IO a) -> BF -> BF -> IO a
bf2 f (BF fptr1) (BF fptr2) =
  withForeignPtr fptr1 (\ptr1 ->
  withForeignPtr fptr2 (\ptr2 ->
    f ptr1 ptr2
  ))




--------------------------------------------------------------------------------
-- Assignment


-- | Indicates if a number is positive or negative.
data Sign = Pos {- ^ Positive -} | Neg {- ^ Negative -}


foreign import ccall "bf_set_nan"
  bf_set_nan :: Ptr BF -> IO ()

-- | Assign NaN to the number.
setNaN :: BF -> IO ()
setNaN (BF fptr) = withForeignPtr fptr bf_set_nan


foreign import ccall "bf_set_zero"
  bf_set_zero :: Ptr BF -> CInt -> IO ()

-- | Assign a zero to the number.
setZero :: Sign -> BF -> IO ()
setZero sig = bf1 (`bf_set_zero` signToC sig)


foreign import ccall "bf_set_inf"
  bf_set_inf :: Ptr BF -> CInt -> IO ()

-- | Assign an infinty to the number.
setInf :: Sign -> BF -> IO ()
setInf sig = bf1 (`bf_set_inf` signToC sig)


foreign import ccall "bf_set_ui"
  bf_set_ui :: Ptr BF -> Word64 -> IO ()

{-| Assign from a word -}
setWord :: Word64 -> BF -> IO ()
setWord w = bf1 (`bf_set_ui` w)


foreign import ccall "bf_set_si"
  bf_set_si :: Ptr BF -> Int64 -> IO ()

{-| Assign from an int -}
setInt :: Int64 -> BF -> IO ()
setInt s = bf1 (`bf_set_si` s)


foreign import ccall "bf_set_float64"
  bf_set_float64 :: Ptr BF -> Double -> IO ()

{-| Assign from a double -}
setDouble :: Double -> BF -> IO ()
setDouble d = bf1 (`bf_set_float64` d)


foreign import ccall "bf_set"
  bf_set :: Ptr BF -> Ptr BF -> IO ()

{-| Assign from another number. -}
setBF :: BF -> BF {-^ This number is changed -} -> IO ()
setBF = bf2 (flip bf_set)


--------------------------------------------------------------------------------
-- Comparisons

foreign import ccall "bf_cmp_eq"
  bf_cmp_eq :: Ptr BF -> Ptr BF -> IO CInt

{-| Check if the two numbers are equal. -}
cmpEq :: BF -> BF -> IO Bool
cmpEq = bfRel bf_cmp_eq


foreign import ccall "bf_cmp_lt"
  bf_cmp_lt :: Ptr BF -> Ptr BF -> IO CInt

{-| Check if the first number is strictly less than the second. -}
cmpLT :: BF -> BF -> IO Bool
cmpLT = bfRel bf_cmp_lt


foreign import ccall "bf_cmp_le"
  bf_cmp_le :: Ptr BF -> Ptr BF -> IO CInt

{-| Check if the first number is less than, or equal to, the second. -}
cmpLEQ :: BF -> BF -> IO Bool
cmpLEQ = bfRel bf_cmp_le


foreign import ccall "bf_cmpu"
  bf_cmpu :: Ptr BF -> Ptr BF -> IO CInt

{-| Compare the absolute values of the two numbers. See also 'cmp'. -}
cmpAbs :: BF -> BF -> IO Ordering
cmpAbs = bfOrd bf_cmpu


foreign import ccall "bf_cmp_full"
  bf_cmp_full :: Ptr BF -> Ptr BF -> IO CInt

{-| Compare the two numbers.  The special values are ordered like this:

      * -0 < 0
      * NaN == NaN
      * NaN is larger than all other numbers
-}
cmp :: BF -> BF -> IO Ordering
cmp = bfOrd bf_cmp_full







foreign import capi "libbf.h bf_is_finite"
  bf_is_finite :: Ptr BF -> IO CInt

foreign import capi "libbf.h bf_is_nan"
  bf_is_nan :: Ptr BF -> IO CInt

foreign import capi "libbf.h bf_is_zero"
  bf_is_zero :: Ptr BF -> IO CInt

{-| Check if the number is "normal", i.e. (not infinite or NaN) -}
isFinite :: BF -> IO Bool
isFinite = bfQuery bf_is_finite

{-| Check if the number is NaN -}
isNaN :: BF -> IO Bool
isNaN = bfQuery bf_is_nan

{-| Check if the given number is a zero. -}
isZero :: BF -> IO Bool
isZero = bfQuery bf_is_zero







foreign import capi "libbf.h bf_neg"
  bf_neg :: Ptr BF -> IO ()

foreign import ccall "bf_add"
  bf_add :: Ptr BF -> Ptr BF -> Ptr BF -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_sub"
  bf_sub :: Ptr BF -> Ptr BF -> Ptr BF -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_mul"
  bf_mul :: Ptr BF -> Ptr BF -> Ptr BF -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_div"
  bf_div :: Ptr BF -> Ptr BF -> Ptr BF -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_divrem"
  bf_divrem :: Ptr BF -> Ptr BF ->
               Ptr BF -> Ptr BF -> LimbT -> FlagsT -> RoundMode -> IO Status

foreign import ccall "bf_fmod"
  bf_fmod :: Ptr BF -> Ptr BF -> Ptr BF -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_remainder"
  bf_remainder :: Ptr BF -> Ptr BF -> Ptr BF -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_remquo"
  bf_remquo :: Ptr BF -> Ptr BF ->
               Ptr BF -> Ptr BF -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_pow_ui"
  bf_pow_ui :: Ptr BF -> Ptr BF -> LimbT -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_pow_ui_ui"
  bf_pow_ui_ui :: Ptr BF -> LimbT -> LimbT -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_round"
  bf_round :: Ptr BF -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_rint"
  bf_rint :: Ptr BF -> LimbT -> FlagsT -> IO Status

foreign import ccall "bf_sqrt"
  bf_sqrt :: Ptr BF -> Ptr BF -> LimbT -> FlagsT -> IO Status



bfArith :: (Ptr BF -> Ptr BF -> Ptr BF -> LimbT -> FlagsT -> IO Status) ->
           BFOpts -> BF -> BF -> BF -> IO Status
bfArith fun (BFOpts prec flags) (BF fa) (BF fb) (BF fr) =
  withForeignPtr fa (\a ->
  withForeignPtr fb (\b ->
  withForeignPtr fr (\r ->
  fun r a b prec flags)))




-- | Negate the number.
fneg :: BF -> IO ()
fneg = bf1 bf_neg

-- | Add two numbers, using the given settings, and store the
-- result in the last.
fadd :: BFOpts -> BF -> BF -> BF -> IO Status
fadd = bfArith bf_add

-- | Subtract two numbers, using the given settings, and store the
-- result in the last.
fsub :: BFOpts -> BF -> BF -> BF -> IO Status
fsub = bfArith bf_sub

-- | Multiply two numbers, using the given settings, and store the
-- result in the last.
fmul :: BFOpts -> BF -> BF -> BF -> IO Status
fmul = bfArith bf_mul

-- | Divide two numbers, using the given settings, and store the
-- result in the last.
fdiv :: BFOpts -> BF -> BF -> BF -> IO Status
fdiv = bfArith bf_div

-- | Compute the modulus of two numbers, and store the result in the last.
fmod :: BFOpts -> BF -> BF -> BF -> IO Status
fmod = bfArith bf_fmod

-- | Compute the reminder of two numbers, and store the result in the last.
frem :: BFOpts -> BF -> BF -> BF -> IO Status
frem = bfArith bf_remainder

-- | Compute the square root of the first number and store the result
-- in the second.
fsqrt :: BFOpts -> BF -> BF -> IO Status
fsqrt (BFOpts p f) = bf2 (\inp res -> bf_sqrt res inp p f)

-- | Round to the nearest float matching the configuration parameters.
fround :: BFOpts -> BF -> IO Status
fround (BFOpts p f) = bf1 (\ptr -> bf_round ptr p f)

-- | Round to the neareset integer.
frint :: BFOpts -> BF -> IO Status
frint (BFOpts p f) = bf1 (\ptr -> bf_rint ptr p f)

fpowWord :: BFOpts -> BF -> Word64 -> BF -> IO Status
fpowWord (BFOpts prec flags) base po x =
  bf2 (\inp res -> bf_pow_ui res inp po prec flags) base x


--------------------------------------------------------------------------------
-- export

foreign import ccall "bf_get_float64"
  bf_get_float64 :: Ptr BF -> Ptr Double -> RoundMode -> IO Status

toDouble :: RoundMode -> BF -> IO (Double, Status)
toDouble r = bf1 (\inp ->
  alloca (\out ->
   do s <- bf_get_float64 inp out r
      d <- peek out
      pure (d, s)
  ))


foreign import ccall "bf_ftoa"
  bf_ftoa :: Ptr CString -> Ptr BF -> CInt -> LimbT -> FlagsT -> IO CSize



toString :: Int -> ShowFmt -> BF -> IO String
toString radix (ShowFmt ds flags) = bf1 (\inp ->
  alloca (\out ->
  do _ <- bf_ftoa out inp (fromIntegral radix) ds flags
     ptr <- peek out
     res <- peekCString ptr
     free ptr
     pure res
  ))




