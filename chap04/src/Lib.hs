module Lib
    ( someFunc
    ) where

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ [] = return  []
mapM' f (x:xs) = do
    x' <- f x
    xs' <- mapM' f xs
    return (x':xs')

-- other definition with composition operator
-- mapM' f (x:xs) = (:) <$> f x <*> mapM' f xs 

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (x:xs) = do
    x' <- x
    xs' <- sequence' xs
    return (x':xs')

-- other definition with composition operator
-- sequence' (x:xs) = (:) <$> x <*> sequence' xs

-- mapM' = sequence' . map 

-- exercice 4.1 

zipWithM' :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM' f x y  =  sequence' (zipWith f x y)

replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' i x = sequence' (replicate i x)

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' f (x:xs) = do
    ok <- f x
    if ok then (:) <$> return x <*> filterM' f xs else filterM' f xs


-- monadic version of combinator
--
-- In Module Control.Monad.Extra 
-- partitionM :: Monad m => (a -> m Bool)  -> [a] -> m ([a], [a])
-- concatMapM :: Monad m => (a -> m [b])   -> [a] -> m [b]
--
-- In Module Control.Monad.Loops
-- takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- dropWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- firstM     :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
-- 

--    
-- Action without value
void' :: Functor m => m a -> m ()
void' = fmap (\_ -> ()) -- use const instead

mapM_' :: Monad m => (a -> m b) -> [a] -> m ()
mapM_' f = void' . mapM' f

filterM_' :: Monad m => (a -> m Bool) -> [a] -> m  ()
filterM_' f = void' . filterM' f

zipWithM_' :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_' f xs = void' . zipWithM' f  xs

someFunc :: IO ()
someFunc = putStrLn "someFunc"


