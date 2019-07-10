import LibBF
import LibBF.Mutable

main :: IO ()
main =
  do print s
     putStrLn (bfToString 2 (showFreeMin (Just 128) <> showRnd NearEven)  d)
     print =<< toRep (bfUnsafeThaw d)

  where
  (d0,s)= bfDiv opts (bfFromWord 1) (bfFromWord 3)
  (d1,_)= bfDiv opts (bfFromWord 1) (bfFromWord 200)
  (d2,_)= bfAdd opts d0 d1
  (d ,_)= bfSqrt opts (bfFromWord 2)
  opts  = precBits 128 <> expBits 11 <> rnd ToZero


