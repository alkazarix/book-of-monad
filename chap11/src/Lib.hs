{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}
module Lib
    ( 
    ) where


import           Control.Monad.Reader hiding (MonadReader, ask, local)
import           Control.Monad.Writer
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Identity 


-- exercice 11.1

type Name = String
type Assignment = [(String, Integer)]

data Op = Add | Multiply | Divide | Subtract
data Expr = Op Op Expr Expr | Literal Integer | Var Name

eval :: Expr -> MaybeT (State Assignment) Integer
eval (Literal n) = return n
eval (Var x)     =  MaybeT $ gets (lookup x)
eval (Op  op x y)   = do 
    x' <- eval x
    y' <- eval y 
    case op of 
        Add      -> return (x' + y')
        Subtract -> return (x' - y')
        Multiply -> return (x' * y')
        Divide    -> if (y' == 0) then fail "could not divide by zero" else return (x' `div` y')

runEval :: Expr -> Assignment -> (Maybe Integer, Assignment)
runEval e = runState . runMaybeT $ eval e

-- exercice 11.2
-- Maybe transformer 

newtype MaybeT' m a = MaybeT' {
    runMaybeT' :: m (Maybe a)
}

instance (Monad m) => Functor (MaybeT' m) where
    fmap f (MaybeT' ma) = MaybeT' $ (fmap . fmap ) f ma

instance (Monad m) => Applicative (MaybeT' m) where
    pure a = MaybeT' $ (pure . pure) a
    MaybeT' mf <*> MaybeT' ma = MaybeT' $ (<*>) <$> mf <*> ma -- ???

instance (Monad m) => Monad (MaybeT' m) where 
    return a = MaybeT' $ (return . return) a
    MaybeT' ma >>= f = MaybeT' $ do 
        a <- ma 
        case a of 
            Nothing -> return Nothing
            Just  a' -> runMaybeT' (f a') 


-- Except tranformer 

newtype ExceptT' e m a = ExceptT' {
    runExcept' :: m (Either e a)
}

instance (Monad m) => Functor (ExceptT' e m) where 
    fmap f (ExceptT' ma) = ExceptT' $ (fmap . fmap) f ma

instance (Monad m) => Applicative (ExceptT' e m) where
    pure a = ExceptT'$ (pure . pure) a
    ExceptT' mf <*> ExceptT' ma = ExceptT' $ (<*>) <$> mf <*> ma 

instance (Monad m) => Monad (ExceptT' e m) where 
    return a = ExceptT' $ (pure . pure) a
    ExceptT' ma >>= f = ExceptT' $ do 
        a <- ma
        case a of 
            Left e   -> return (Left e)
            Right a' -> runExcept' (f a')

-- Reader transformer

newtype ReaderT' r m a = ReaderT' {
    runReaderT' :: r -> m a
}


instance (Monad m) => Functor (ReaderT' r m) where 
    fmap f (ReaderT' ma) = ReaderT' $ (fmap . fmap) f ma

instance (Monad m) => Applicative (ReaderT' r m) where
    pure a = ReaderT' $ (pure . pure) a
    ReaderT' mf <*> ReaderT' ma = ReaderT' $ (<*>) <$> mf <*> ma 



instance (Monad m) => Monad (ReaderT' r m) where 
    return a = ReaderT' $ (pure . pure) a
    ReaderT' ma >>= f = ReaderT' $ \r -> do 
        a <- ma r
        runReaderT' (f a) r 

-- Writer transformer

newtype WriterT' w m a = WriterT' {
    runWriterT' :: m (a, w)
}


instance (Monad m) => Functor ( WriterT' w m) where 
    fmap f ( WriterT' ma) =  WriterT' $ fmap f' ma
        where f' (a, w) = (f a, w)

instance (Monad m, Monoid w) => Applicative (WriterT' w m) where
    pure a = WriterT' $ pure (a, mempty)
    WriterT' mf <*> WriterT' ma = WriterT' $ do 
        (f, w) <- mf
        fmap (\(a,w) -> (f a, w)) ma 

instance (Monad m, Monoid w) => Monad (WriterT' w m) where
    return a = WriterT' $ return (a, mempty)

    WriterT' ma >>= f = WriterT' $ do 
        (a , w) <- ma
        (b , w') <- runWriterT' (f a)
        return $ (b, w `mappend` w')
        

-- exercise 11.3 

toIdentity :: a -> Identity a
toIdentity = pure

fromIdentity :: Identity a -> a
fromIdentity (Identity x) = x

-- exercise 11.4 

toMaybeT :: Maybe a -> MaybeT Identity a
toMaybeT = \case
  Just x -> return x
  Nothing -> MaybeT $ return Nothing

fromMaybeT :: MaybeT Identity a -> Maybe a
fromMaybeT = runIdentity . runMaybeT


-- 11.2 class of monad, mtl

    
-- type Evaluator = MaybeT Reader Assignement Integer 
-- eval :: Expr -> Evaluator 
-- eval (Var x) = do 
--                   a <- ask //require to read enviroment 
--                   (...)
--
-- problem :  ask function has type (ask ::  Reader r r), but the calculation occure on 
--            MaybeT Reader r a  monad
--
-- solution:  class of monad , MonadReader  

class Monad m => MonadReader r m | m -> r where 
    ask :: m r
    local :: (r -> r) -> m a -> m a

instance Monad m => MonadReader r (ReaderT r m ) where
    ask =  ReaderT  return
    local f m = ReaderT $ runReaderT m . f 


instance MonadReader r m => MonadReader r (MaybeT m) where
    ask = MaybeT $ runMaybeT ask
    local f m  = MaybeT $ runMaybeT (local f m)

-- each monad have her corresponding type class that  represent primitive of the operation of the monad
-- example: MonadError, MonadIO, MonadWriter, MonadReader, MonadState ...

-- the package mtl provide for us the correct implementation of monad type class for the corresponding 
-- monad transformer 
-- example:
--    ---------------------------------------------------------
--    |    type class          |  transformer                 |
--    ---------------------------------------------------------
--    |  MonadError  e m       | ExcepT e m                   |
--    |  MonadState  s m       | StateT s m                   |
--    ---------------------------------------------------------




        

        
        