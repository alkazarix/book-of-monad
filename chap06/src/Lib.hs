{-# LANGUAGE TupleSections #-}

module Lib
    (
    ) where

import Data.Semigroup

-- 
-- Package State Monad
-- 

newtype State s a = State {
    runState :: s -> (a, s)
}


-- exercise 6.1
-- Implement Functor, Applicative, Monad for State    

instance Functor (State s) where
    fmap f (State sa) = State $ \s -> let (a, s') = sa s in (f a, s')

instance Applicative (State s) where
    pure a = State (a, )

    (<*>) (State fab) (State sa) = State $ \s ->
        let (f, s') = fab s
            (a, s'') = sa s'
        in (f a, s'')

instance Monad (State s) where
    return a = State (a, )

    (>>=) (State sa) f = State $ \s ->
        let (a, s') = sa s
        in runState (f a) s'

-- 

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

-- exercise 6.2 
-- modify in term of get, and put 

modify :: (s -> s) -> State s ()
modify f = do
    s <- get
    put (f s)

-- 

evalState :: State s a -> s -> a
evalState = (fst .) . runState

execState :: State s a -> s -> s
execState = (snd .) . runState


-- 
-- Package Reader Monad
-- 

newtype Reader r a = Reader {
    runReader :: r -> a
}

instance Functor (Reader r)  where
    fmap f (Reader ra) = Reader $  f . ra

instance Applicative (Reader r) where
    pure = Reader . const

    (<*>) (Reader rab) (Reader ra) = Reader $ \r -> rab r $ ra r

instance Monad (Reader r) where
    return = Reader . const

    (>>=) (Reader ra) f = Reader $ \r -> runReader (f (ra r)) r

ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = f <$> ask

withReader :: (r -> s) -> Reader s a -> Reader r a
withReader rs (Reader sa) = Reader $ sa . rs


-- 
-- Package Writer Monad
-- 

newtype Writer w a = Writer {
    runWriter :: (w, a)
}

instance Functor (Writer w) where
    fmap f (Writer wa) = Writer (fst wa, f $ snd wa)

instance Monoid w => Applicative (Writer w) where 
    pure = Writer . (mempty,)

    (<*>) (Writer wab) (Writer wa) = Writer $ wab <*> wa 

instance Monoid w => Monad (Writer w) where 
    return = Writer . (mempty,)

    (>>=) (Writer wa) f  = Writer $ 
        let Writer wb = (f . snd) wa 
        in (fst wa <> fst wb , snd wb) 

tell :: w -> Writer w ()
tell = Writer . (, ())

-- example writer 

data Expr =
    Lit Float
  | Add Expr Expr
  | Divide Expr Expr

eval :: Expr -> Float
eval (Lit n) = n
eval (Add e g) = eval e + eval g
eval (Divide e g) =
  case (eval e, eval g) of
    (_, 0) -> 0
    (u, v) -> u / v

eval' :: Expr -> Writer [String] Float
eval' (Lit n) = pure n
eval' (Add e g) = (+) <$> eval' e <*> eval' g
eval' (Divide e g) = do
  x <- eval' e
  y <- eval' g
  case (x, y) of
    (_, 0) -> do
      tell ["Divide by zero!"]
      return 0
    (u, v) -> pure $ u / v