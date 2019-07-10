import LibBF

main :: IO ()
main =
  do print s
     putStrLn (bfToString 10 (showFreeMin (Just 53) <> showRnd NearEven)  d)

  where
  (d,s) = bfDiv opts (bfFromWord 3) (bfFromWord 10)
  opts  = precBits 53 <> expBits 11 <> rnd ToZero


