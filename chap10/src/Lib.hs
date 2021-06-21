{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
    
module Lib
    (
    ) where

import Control.Monad.Reader
import Data.Functor.Identity
import Control.Applicative

type Name = String
type Assignement = [(String, Integer)]

data Op = Add | Multiply | Divide | Subtract
data Expr = Op Op Expr Expr | Literal Integer | Var Name

eval :: Expr -> Assignement -> Maybe Integer
eval expr assignement = case expr of
    Literal n -> Just n
    Var x     -> lookup x assignement
    Op o x y  -> do
        u <- eval x assignement
        v <- eval y assignement
        case o of
            Add       -> return (u + v)
            Subtract  -> return (u - v)
            Multiply  -> return  (u * v)
            Divide    -> if v == 0 then Nothing  else return (u `div` v)

eval' :: Expr -> Reader Assignement (Maybe Integer)
eval' (Literal n) = return (Just n)
eval' (Var x)     = lookup  x <$> ask
eval' (Op o x y)  = do
        u <- eval' x
        v <- eval' y
        case (u, v) of
            (Nothing , _) -> return Nothing
            (_, Nothing)  -> return Nothing
            (Just u', Just v') -> case o of
                Add       -> return (Just (u' + v'))
                Subtract  -> return (Just (u' - v'))
                Multiply  -> return  (Just (u' * v'))
                Divide    -> if v' == 0 then return Nothing  else return (Just (u' `div` v'))


newtype Evaluator a = Evaluator {
    runEvalualor :: ReaderT Assignement Identity (Maybe a)
}

instance Functor Evaluator where 
    fmap f (Evaluator e) = Evaluator . ReaderT $ \a -> 
        runReaderT e a >>= \case 
            Nothing -> return Nothing 
            Just  y  -> return (Just (f y))

instance Applicative Evaluator where 
    pure x = Evaluator . return $ Just x 
    (Evaluator f) <*> (Evaluator e) = Evaluator . ReaderT $ \a -> 
        runReaderT f a >>= \case 
            Nothing -> return Nothing 
            Just f' -> runReaderT e a >>= \case 
                Nothing  -> return Nothing 
                Just  e' -> return (Just (f' e'))


instance Monad Evaluator where 
    return x = Evaluator . return $ Just x
    fail _ = Evaluator $ return Nothing
    (Evaluator e) >>= f =
        Evaluator . ReaderT $ \a ->
            runReaderT e a >>= \case
                Nothing -> return Nothing
                Just y ->   let Evaluator e' = f y in runReaderT e' a

eval'' :: Expr -> Evaluator Integer
eval'' (Literal n) = return n 
eval'' (Var x)     =  do 
        a <- Evaluator $ Just <$> ask
        case lookup x a of
            Nothing -> fail $ x ++ " is not in the assignments"
            Just v' -> return v'
eval'' (Op o x y)  = do 
    u <- eval'' x 
    v <- eval'' y 
    case o of 
        Add        -> return (u + v)
        Subtract   -> return (u - v)
        Multiply   -> return (u * v)
        Divide     -> if v == 0 then fail "could not divide by zero"  else return $ (u `div` v)


newtype (f :.: g) a = Compose (f (g a))

instance (Functor f, Functor g) => Functor (f :.: g) where
    fmap func (Compose a) = Compose  $ (fmap .fmap) func a

instance (Foldable (f :.: g), Traversable f, Traversable g) =>
         Traversable (f :.: g) where
  traverse f (Compose g) = Compose <$> (traverse . traverse) f g

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure = Compose . pure . pure
  Compose f <*> Compose x = Compose $ (<*>) <$> f <*> x

instance (Alternative f, Alternative g) => Alternative (f :.: g) where
   empty = Compose $ empty
   Compose x <|> Compose y = Compose $ x <|> y


newtype Reader' r a = Reader' {
    runReader' :: r -> a
}

newtype Writer' w a = Writer' {
    runWriter' :: (a, w)
}

swapMaybe :: (Monad m) => Maybe (m a) -> m (Maybe a) 
swapMaybe  Nothing  = return Nothing
swapMaybe  (Just x) = Just <$> x
  
-- exercice 10.1
swapWriter :: (Monad m) => Writer' w (m a) -> m (Writer' w a)
swapWriter (Writer' (ma, w)) = ma >>= (\a -> return $ Writer' $ (a, w))

swapList :: (Monad m) => [m a] -> m [a]
swapList [] = return []
swapList (x:xs) = (:) <$> x <*> (swapList xs)

-- dual
swapReader :: (Monad m) => m (Reader' r a) -> Reader' r (m a)
swapReader c = Reader' $ \r -> c >>= \rra -> return $ runReader' rra r

newtype Listed m a =
  Listed
    { unListed :: [m a]
    } deriving Show


instance (Monad m) => Functor (Listed m) where
    fmap f (Listed xs) = Listed $ (fmap . fmap) f xs

-- exercice 10.2
instance (Monad m) => Applicative (Listed m) where
    pure a = Listed $ (pure . pure) a
    (Listed fs) <*> (Listed xs) = Listed $ (<*>) <$> fs <*> xs

