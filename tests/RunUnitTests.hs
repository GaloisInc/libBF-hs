{-# Language BlockArguments #-}
module Main(main) where

import Data.Foldable(traverse_)
import System.Exit(exitFailure)
import System.IO(hPutStrLn,stderr)
import Control.Monad(unless)

import LibBF


main :: IO ()
main =
  do putStrLn $ bfToString 16 (showFree Nothing) bfNaN
     print $ bfFromString 10 (expBits 3 <> precBits 2 <> rnd ToZero) "0.001"
     print $ bfFromString 10 (expBits 3 <> precBits 2 <> rnd ToZero) "1.0e200"
     dblTest "+" (+) (bfAdd (float64 NearEven)) 1 2
     dblTest "/" (/) (bfDiv (float64 NearEven)) 1 0
     traverse_ (\bf -> bfSubnormalTest bf False)
       [bfPosZero, bfFromInt 1, bfFromInt 0, bfNaN, bfNegInf, bfPosInf]

check :: String -> Bool -> IO ()
check x b = unless b
              do hPutStrLn stderr ("Test failed: " ++ x)
                 exitFailure

dblTest ::
  String ->
  (Double -> Double -> Double) ->
  (BigFloat -> BigFloat -> (BigFloat, Status)) ->
  Double -> Double -> IO ()
dblTest op opD opBF x y =
  case z1 of
    Left err -> check (lab ("status: " ++ err)) False
    Right a  -> check (lab (show a)) (z == a)
  where
  lab err = unwords [ show x, op, show y, "=", show z, err ]

  z  = opD x y
  z1 = case opBF (bfFromDouble x) (bfFromDouble y) of
        (res,_) ->
          case bfToDouble NearEven res of
            (res1,Ok) -> Right res1
            (_, s)    -> Left ("result: " ++ show s)

-- Check that calling bfIsSubnormal on a BigFloat value returns the expected
-- result.
bfSubnormalTest :: BigFloat -> Bool -> IO ()
bfSubnormalTest bf expected =
  check ("bfIsSubnormal (float32 NearEven) " ++ show bf)
        (bfIsSubnormal (float32 NearEven) bf == expected)
