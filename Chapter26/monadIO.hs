module MyMonadIO where

import Control.Monad.IO.Class
import Control.Monad.Trans.Identity

instance (MonadIO m) => MonadIO (IdentityT m) where
  liftIO = IdentityT . liftIO
