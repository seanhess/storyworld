module Generate where


import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Random          (randomRIO)


randomElement :: MonadIO m => [a] -> m a
randomElement [] = error "empty list"
randomElement [a] = pure a
randomElement as = do
  i <- liftIO $ randomRIO (0, length as - 1)
  pure $ as !! i

