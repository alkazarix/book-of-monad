{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( 
    ) where

import Control.Applicative
import           Control.Monad
import           Control.Monad.Logic

--     
-- data Either 
-- exercise 7.1 

data Either' e r = Left' e | Right' r 

instance Functor (Either' e) where 
    fmap _ (Left' e) = Left' e
    fmap f (Right' r) = Right' (f r)

instance Applicative (Either' e) where 
    pure  = Right' 

    (<*>) f (Left' e) = Left' e
    (<*>) f (Right' r) = case f of 
        Left' e   -> Left' e
        Right' f' -> Right' (f' r)

instance Monad (Either' e) where 
    return = Right'

    (>>=) (Left' e) _ = Left' e
    (>>=) (Right' r) f = f r 

-- 
-- Alternative
--

--
-- class Applicative f => Alternative f where
--     empty ::  fa 
--    (<|>)  :: f a -> f a -> f a 
-- 

-- usage 

data Person = Person {
    name :: String,
    age  :: Integer
}

validateNameEnglish :: String -> Maybe String 
validateNameEnglish = undefined

validateNameUS :: String -> Maybe String 
validateNameUS = undefined 

validateNameFrench :: String -> Maybe String 
validateNameFrench = undefined 

validateName :: String -> Maybe String 
validateName s = validateNameEnglish s <|> validateNameFrench s <|> validateNameUS s

validateAge :: Integer -> Maybe Integer 
validateAge = undefined 

validatePerson :: String -> Integer -> Maybe Person 
validatePerson name age = Person <$> validateName name <*> validateAge age

-- exercise 7.2 

instance Monoid e => Alternative (Either' e) where 
    empty = Left' mempty -- e neutral element

    (<|>) (Left' e) ea = ea 
    (<|>) (Right' r) _ = Right' r

-- 
-- guard 
-- 

-- definition
-- 
-- guard :: Alternative f => Bool -> m()
-- guard True = pure ()
-- guard False = mempty

-- usage 

validateAge' :: Integer -> Maybe Integer 
validateAge' age = do
    guard (age >= 18)
    return age

--
-- Logic Programming 
-- 

type People = String

people :: [People]
people = ["Alejandro", "Elena", "Quipue", "John", "Mary", "Tom"]

parentChildRelationships :: [(People, People)]
parentChildRelationships =
  [ ("Alejandro", "Quipue")
  , ("Elena"    , "Quipue")
  , ("John"     , "Mary")
  , ("John"     , "Tom")
  , ("Mary"     , "Tim")
  ]

grandparentGrandChildRelationships :: [(People, People)]
grandparentGrandChildRelationships = do 
    (grandp , parent) <- parentChildRelationships
    (parent', grandc) <- parentChildRelationships
    guard (parent == parent')
    return (grandp, grandc)

-- exercise 7.3 

siblingsRelationships :: [(People, People)]
siblingsRelationships =  do 
    (parent, child) <- parentChildRelationships
    (parent', child') <- parentChildRelationships
    guard (parent == parent' && child /= child')
    return (child, child')


-- without monad logic 
sums :: [Integer] -> [(Integer, Integer, Integer)]
sums ns = do
  x <- ns
  y <- ns
  z <- ns
  guard (x + y == z)
  return (x, y, z)

pyts :: [Integer] -> [(Integer, Integer, Integer)]
pyts ns = do
  x <- ns
  y <- ns
  z <- ns
  guard (x ^ 2 + y ^ 2 == z ^ 2)
  return (x, y, z)

triplets = liftA2 (<|>) sums pyts

-- with monad logic 

list :: [a] -> Logic a
list = msum . map return

fairTriples :: [Integer] -> Logic (Integer, Integer, Integer)
fairTriples ns =
  list ns >>- \x -> list ns >>- \y -> list ns >>- \z -> return (x, y, z)

sums' :: [Integer] -> Logic (Integer, Integer, Integer)
sums' ns =
  fairTriples ns >>= \(x, y, z) -> guard (x + y == z) >> return (x, y, z)

pyts' :: [Integer] -> Logic (Integer, Integer, Integer)
pyts' ns = fairTriples ns
  >>= \(x, y, z) -> guard (x ^ 3 + y ^ 3 == z ^ 3) >> return (x, y, z)

triples' = liftA2 (<|>) sums' pyts'

--
-- Monad Error
-- 

class Monad m => MonadError e m | e -> m where 
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a

data NameReason = NameTooLong | UnKnowCharacter 

cut :: String -> String 
cut = undefined 

normalize :: String -> String 
normalize = undefined 

validateName' :: String -> m  String 
validateName' = undefined 

vn  :: MonadError NameReason m  => String -> m String 
vn  name  = validateName' name `catchError`(\e -> case e of 
      NameTooLong -> vn (cut name)
      UnKnowCharacter -> vn (normalize name)
  )

-- exercise 7.4 


instance MonadError e (Either e) where 
  throwError  = Left

  catchError (Right a) _ = Right a
  catchError (Left e)  f = f e 
