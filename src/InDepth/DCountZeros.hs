module InDepth.DCountZeros where

import Data.Foldable (traverse_)
import Control.Monad (when)
import Control.Monad.ST
import Data.STRef

countZerosST :: [Int] -> Int
countZerosST xs = runST $ do
    c <- newSTRef 0
    traverse_ (\x -> when (x==0) $ inc c) xs
    readSTRef c
  where
    inc c = modifySTRef' c (+1)