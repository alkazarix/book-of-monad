{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
module Lib
    ( 
    ) where


-- 14.2 initial style 

data Free f a =  
        Free (f (Free f a))
    |   Pure a


instance Functor f => Functor (Free f) where
    fmap f (Pure a) = Pure $ f a
    fmap f (Free a) = Free (fmap (fmap f) a)

instance Functor f => Applicative (Free f) where
    pure a = Pure a
    Pure f <*> Pure a = Pure $ f a
    Pure f <*> Free a = Free (fmap (fmap f) a)
    Free f <*>  x = Free (fmap (<*> x) f)

instance Functor f => Monad (Free f) where  
    return a = Pure a
    Pure a >>= f = f a
    Free a >>= f = Free (fmap (>>= f) a)

liftF :: (Functor f) => f a -> Free f a
liftF = undefined 

data FSError =  IOError

data FSF r =  
      WriteFile FilePath String (Either FSError () -> r)
    | ReadFile  FilePath (Either FSError String -> r)
    deriving (Functor)

data RandomGenF r = Random Int Int (Int -> r)
    deriving (Functor)

data (f :+: g) a = InL (f a) | InR (g a)

-- exercice 14.1 
instance (Functor f, Functor g) => Functor (f :+: g)  where
    fmap h (InL fa) = InL $ h <$> fa
    fmap h (InR ga) = InR $ h <$> ga 

-- FSRandomF is a functor, because FSF and RandomGenF are
-- FSRandom is a monad 
type FSRandomF  = FSF  :+: RandomGenF 
type FSRandom = Free FSRandomF

-- smart constructor 
writeFile :: FilePath -> String -> FSRandom  (Either FSError ())
writeFile fp contents =  liftF (InL $ WriteFile fp contents id)


readFile :: FilePath -> FSRandom (Either FSError String)
readFile fp = liftF (InL $ ReadFile fp id)

random :: Int -> Int -> FSRandom Int 
random lower upper = liftF (InR $ Random lower upper id)

-- Interpretation will take the following shape, where {M} is the Monad to transform to
-- interpret = foldFree interpret'
--   where
--     interpret' :: (f :+: g) a -> {M} a
--     interpret' = _a

class f :<: g where 
    inject :: f a -> g a 

instance f :<: f where 
    inject = id 

instance f :<: (f :+: g) where 
    inject = InL

instance (f :<: h) => f :<: (g :+: h) where 
    inject = InR . inject

-- rewrite smart constructor 

writeFile' :: (Functor f, (FSF :<: f)) => FilePath -> String -> Free f (Either FSError ())
writeFile' fp contents =  liftF ( inject $ WriteFile fp contents id)

readFile' :: (Functor f , (FSF :<: f)) => FilePath -> Free f (Either FSError String)
readFile' fp = liftF (inject $ ReadFile fp id)

random' :: (Functor g, (RandomGenF :<: g)) => Int -> Int -> Free g Int 
random' lower upper =  liftF (inject $ Random lower upper id)

randomWrite' :: (Functor f, FSF :<: f, RandomGenF :<: f) => FilePath -> Free f (Either FSError ())
randomWrite' path = do
  number <- random' 1 100
  writeFile' path (show number)
