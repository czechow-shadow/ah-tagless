{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib where

import Protolude

import Control.Monad.Trans.Control
import qualified Data.Text as T

--import System.IO.Error
import qualified Control.Exception.Lifted as L
import qualified System.Timeout.Lifted as L

import Pipes
import qualified Pipes.Prelude as P
  

type ErrText = Text

  

class Monad m => MonadLogger m where
  logM :: Text -> m ()

-- transformers: 
instance MonadLogger m => MonadLogger (ExceptT e m) where
  logM = lift . logM
  

newtype MyIO a = MyIO (IO a) deriving (Functor, Applicative, Monad)

instance MonadLogger IO where
  logM xs = putText $ "Log: " <> xs

instance MonadLogger MyIO where
  logM xs = MyIO $ putText $ "here " <> xs

readFromDB :: (MonadLogger m, MonadError ErrText m) => m ()
readFromDB = do
  logM "test"

data Conn = Conn
data Statement = Statement Int
data DbError = DbError
             | DbStError
data StError = StError
data Error = Error 

-- provided by a library
withDb :: Text -> (Conn -> IO a) -> IO a
withDb _txt f = do
  let conn = Conn
  bracket_ (putText "Opening database") (putText "Closing database") $ f conn

-- withDb' :: (MonadIO m, MonadError DbError m) => Text -> (Conn -> IO a) -> m a
-- withDb' t f = handleIOEx (\(_ :: IOException) -> DbError) (withDb t f)

-- handleIOEx :: (Exception e, MonadError e' m, MonadIO m) => (e -> e') -> IO a -> m a
-- handleIOEx f action = liftIO (try action) >>= \case
--   Right v -> pure v
--   Left (e :: e) -> throwError $ f e

withDbLifted :: (MonadBaseControl IO m, MonadError DbError m, MonadLogger m)
             => Text -> (Conn -> m a) -> m a
withDbLifted txt f' = do
  logM "Start withDbLifted"
  liftBaseOp (withDb txt) f'
    `L.catch` (\(_ :: IOException) -> throwError DbError)
    `L.finally` logM "Stop withDbLifted"


-- from a library
withStatement :: Text -> Conn -> (Statement -> IO a) -> IO a
withStatement x _ f =
  bracket_ (putText "Start withStatement") (putText "Stop withStatement") $ do
    -- threadDelay $ 1000 * 1000 * 2
    f $ Statement $ T.length x
    
    -- throwIO $ userError "Failing statement with an exception"
  
withStatementLifted :: (MonadBaseControl IO m, MonadError StError m, MonadLogger m)
                    => Text -> Conn -> (Statement -> m a) -> m a
withStatementLifted statement conn f' = do
  logM "Start withStatmentLifted"
  liftBaseOp (withStatement statement conn) f'
    `L.catch` (\(_ :: IOException) -> throwError StError)
    `L.finally` logM "Stop withStatmentLifted"


go :: IO ()
go = do
  res <- L.timeout (1000 * 1000 * 1) $ runExceptT $ 
    withDbLifted "Wania" $ \conn -> do
      withExceptT (const DbStError) $
        withStatementLifted "select * from dual" conn $ \st -> do
          P.fold (flip (:)) [] reverse $ mkProducer st
          
  case res of
    Just (Right v) -> putText $ "Success: " <> show v
    Just (Left _) -> putText "Failure"
    Nothing -> putText "Operation timed out"

mkProducer :: (MonadLogger m, Monad m)
           => Statement -> Producer (Either ErrText Text) m ()
mkProducer _st = do
  yield $ Right "One"
  yield $ Right "Two"
  yield $ Right "Three"
  logM "Finishing producer"

instance MonadLogger m => MonadLogger (Pipes.Proxy p a b r m) where
  logM = lift . logM

