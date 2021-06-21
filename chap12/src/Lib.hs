{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}

module Lib
    ( 
    ) where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.ST
import System.IO hiding (withFile)

-- 12.1 monad trans and lift

class MonadTrans t where
    lift :: Monad m => m a -> t m a

instance MonadTrans MaybeT where 
    lift x = MaybeT $ Just <$> x

instance MonadTrans (ReaderT r) where 
    lift x = ReaderT $ (\_ -> x)

f1 :: Maybe Int 
f1 = return 5 

f2 :: Int -> ReaderT String Maybe Bool
f2  n = return (n == 0) 

-- do not compile - cannot match Maybe with ReaderT instance
-- op = do 
--         n <- f1
--         f2 n 
-- correct way 
op = do 
        n <- lift f1
        f2 n 

-- 12.2 monad base

class (Monad b, Monad m) => MonadBase b m | m -> b where 
    liftBase :: b a -> m a 

newtype IOWrap a =
  IOWrap
    { runIOWrap :: IO a
    } deriving (Functor, Applicative, Monad)

newtype STWrap s a =
  STWrap
    { runSTWrap :: ST s a
    } deriving (Functor, Applicative, Monad)

instance MonadBase IOWrap IOWrap where
  liftBase = id

instance MonadBase (STWrap s) (STWrap s) where
  liftBase = id


-- 12.3 lifting function with callback 

-- exercice 12.3 
class (Monad m) =>  MonadUnliftIO m where 
    withRunInIO :: ((forall a. m a -> IO a ) -> IO b -> m b)


withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile = undefined

withFileUnlift :: MonadUnliftIO m => FilePath -> IOMode -> (Handle -> m r) -> m r
--withFileUnlift fp im act = withRunInIO $ \run -> withFile fp im (run . act)
withFileUnlift = undefined