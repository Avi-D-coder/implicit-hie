module Hie.Logger where

import           Control.Monad.IO.Class
import qualified Data.Text as T
import           System.Log.Logger

-- ---------------------------------------------------------------------

setupLogger :: MonadIO m => Priority -> m ()
setupLogger p = liftIO $ updateGlobalLogger loggerName
                          (setLevel p)

loggerName :: String
loggerName = "gen-hie"

-- ---------------------------------------------------------------------

logm :: MonadIO m => String -> m ()
logm s = liftIO $ infoM loggerName s

debugm :: MonadIO m => String -> m ()
debugm s = liftIO $ debugM loggerName s

warningm :: MonadIO m => String -> m ()
warningm s = liftIO $ warningM loggerName s

errorm :: MonadIO m => String -> m ()
errorm s = liftIO $ errorM loggerName s

-- ---------------------------------------------------------------------
