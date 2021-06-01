{-# Language BlockArguments #-}
{-# Language LambdaCase #-}
module Main(main) where

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
     dblTest "atan2" atan2 (bfAtan2 (float64 NearEven)) 1 2
     dblTest "atan2" atan2 (bfAtan2 (float64 NearEven)) 2 1
     checkPredicate "sin" (bfSin (float256 NearEven)) (bfIsZero) (bfFromDouble 0)
     checkPredicate "exp" (bfExp (float256 NearEven)) (== (bfFromDouble 1)) (bfFromDouble 0)

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

checkPredicate ::
  Show a =>
  String ->
  (a -> (BigFloat, Status)) ->
  (BigFloat -> Bool) ->
  a ->
  IO ()
checkPredicate opName opBF predicate input = do
  check
    ("Status '" ++ show bfStatus ++ "' not OK for " ++ describeOp)
    (bfStatus == Ok)
  check
    ("Test predicate failed on result " ++ show bfRes ++ " on " ++ describeOp)
    (predicate bfRes)
  where
    (bfRes, bfStatus) = opBF input
    describeOp = opName ++ "(" ++ show input ++ ")"
