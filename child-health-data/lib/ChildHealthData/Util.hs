module ChildHealthData.Util
  ( accumulateM
  ) where

import Control.Monad

accumulateM :: Monad m => b -> [a] -> (b -> a -> m b) -> m b
accumulateM b as f = foldM f b (reverse as)
