{-# Language BlockArguments #-}
module Main(main) where

import System.Exit(exitFailure)
import System.IO(hPutStrLn,stderr)
import Control.Monad(unless)

import LibBF


main :: IO ()
main =
  do dblTest "+" (+) (bfAdd (float64 NearEven)) 1 2
     dblTest "/" (/) (bfDiv (float64 NearEven)) 1 0

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



