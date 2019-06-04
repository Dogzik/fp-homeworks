module Task4
  ( sqrtIntJS
  , fibJS
  ) where

import           Task2 (MonadJS (..), Val (..), VarJS (..), ToVal(..))

sqrtIntJS :: MonadJS m s => m Val -> m Val
sqrtIntJS =
  sFun1 $ \x res ->
    sWithVar (0 :: Int) $ \l ->
      sWithVar (0 :: Int) $ \r ->
        sWithVar (1 :: Int) $ \one ->
          r @=@ x @#
          sWhile
             (sReadVar r @-@ sReadVar l @>@ sReadVar one)
             (sWithVar Undefined $ \m ->
                sWithVar (2 :: Int) $ \two ->
                  (m @=@ (sReadVar r @+@ sReadVar l) @/@ sReadVar two) @#
                  sIf
                    (sReadVar m @*@ sReadVar m @>@ x)
                    (r @=@ sReadVar m)
                    (l @=@ sReadVar m)
              ) @#
          res @=@ sReadVar l


fibJS :: MonadJS m s => m Val -> m Val
fibJS = sFun1 $ \n res ->
          res @= (1 :: Int) @#
          sWithVar (1 :: Int) (\a ->
            sWithVar (1 :: Int) (\b ->
              sWithVar (2 :: Int) (\cur ->
                sWhile 
                  (n @>@ sReadVar cur)
                  (
                    res @=@ sReadVar a @+@ sReadVar b @#
                    a @=@ sReadVar b @#
                    b @=@ sReadVar res @#
                    sWithVar (1 :: Int) (\one -> 
                      cur @=@ (sReadVar cur @+@ sReadVar one)
                    )
                  )
              )
            )
          )


