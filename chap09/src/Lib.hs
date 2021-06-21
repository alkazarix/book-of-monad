{-# LANGUAGE RankNTypes #-}
module Lib
    ( 
    ) where


import Control.Exception 
import System.IO hiding (withFile)
import Control.Monad.Managed
import Control.Monad.Trans.Resource (runResourceT, allocate, release)

-- 9.1 bracket 

-- bracket ::  IO r -> (r -> IO b) -> (r -> IO a) -> IO a  
-- bracket =  aquire    release        use          
--
-- in c# , similar as using  or tr/catch/finaly block
-- using(r = aquire)  {
--    use r      
-- }
-- 
-- if f#, similar as using/dispose 
-- let using r f = try f(r) finally r.Dispose()

--
-- example: dealing with file
--
withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r 
withFile fp mode = bracket (openFile fp mode) hClose


-- 9.2 contination

newtype Cont r a = Cont {
    runCont :: (a -> r) -> r
}

-- exercice 9.1
toCont :: a -> (forall r. Cont r a)
toCont a = Cont $ \r -> r a

fromCont :: (forall r. Cont r a) -> a
fromCont (Cont f) = f id


instance Functor (Cont r) where
    -- f   :: a -> b 
    -- arr :: (a -> r) -> r
    -- x   :: (b -> r)
    fmap f (Cont ar) = Cont $ \x -> ar (x . f)


instance Applicative (Cont r) where
    pure a = Cont $ \x -> x $ a

    -- abr  :: ((a -> b) -> r) -> r
    -- ar :: (a -> r) -> r
    -- x   :: (b -> r)
    Cont abr <*> Cont ar = Cont $ \x -> abr $ \f -> ar (x . f)

instance Monad (Cont r) where 
    return a = Cont $ \x -> x $ a

    -- ar  :: (a -> r) -> r 
    -- f   ::  a  -> (Cont) ((b -> r) -> r)
    -- x   ::  (b -> r)
    (Cont ar) >>= f = Cont $ \x -> ar $ \a -> runCont (f a) $ x 


-- think continuation as javascript promise 
-- 
-- in js 
-- http.get("http://google.fr").then( ..... ) 
-- 
-- in haskell 
-- get :: String -> (Result -> IO ()) -> IO()
-- or 
-- get :: String -> Cont () Result 
--
-- do 
--   r1 <- get "http://google.fr"
--   r2 <- get "http://bing.fcom"
--   (..)


-- avoid "callback hell" with managed resource 

doWork :: Handle -> Handle -> IO()
doWork = undefined  

processWithCallBack :: FilePath -> FilePath -> IO()    
processWithCallBack inFile outFile = 
    withFile inFile ReadMode $ \inHandle -> 
        withFile outFile WriteMode $ \ouHandle -> 
            doWork inHandle ouHandle

process :: FilePath -> FilePath -> IO()   
process inFile outFile = runManaged $ do 
    inHandle <- managed (withFile inFile ReadMode)
    outHandle <- managed (withFile outFile WriteMode)
    liftIO $ doWork inHandle outHandle


-- 9.3 elarly release
--
-- early release functionality in package Control.Monad.Trans.Resource
-- 

zipFiles :: FilePath -> FilePath -> FilePath -> IO()
zipFiles fin1 fin2 outf = 
    withFile fin1 ReadMode  $ \in1 -> 
        withFile fin2 ReadMode $ \in2 -> 
            withFile outf WriteMode $ \out -> 
                go in1 in2 out
    where 
        go in1 in2 out = do 
            eof1 <- hIsEOF in1 
            if eof1 then end in2 out 
            else do 
              eof2 <- hIsEOF in2
              if eof2 then end in1 out  
              else do 
                hGetChar in1 >>= hPutChar out 
                hGetChar in2 >>= hPutChar out 
                go in1 in2 out 
        
        end _in out = do
           eof <- hIsEOF  _in 
           if eof then return ()
           else do 
            hGetChar _in >>= hPutChar out 
            end _in out 

zipFiles' :: FilePath -> FilePath -> FilePath -> IO() 
zipFiles' fin1 fin2 fout = runResourceT $ do 
    (rin1, in1) <- allocate (openFile  fin1 ReadMode) hClose
    (rin2, in2) <- allocate (openFile  fin2 ReadMode) hClose
    (rout, out) <- allocate (openFile  fin1 WriteMode) hClose
    liftIO (go rin1 in1 rin2 in2 rout out)
    where 
        go rin1 in1 rin2 in2 rout out = do
            eof1 <- liftIO $ hIsEOF in1 
            if eof1 then release rin1 >> end in2 out rout 
            else do
             eof2 <- liftIO $ hIsEOF in2 
             if eof2 then release rin2 >> end in1 out rout 
             else do 
                hGetChar in1 >>= hPutChar out 
                hGetChar in2 >>= hPutChar out 
                go rin1 in1 rin2 in2 rout out

        end in_ out rout = do 
            eof <- liftIO $ hIsEOF in_
            if eof then release rout >> return()
            else do
                hGetChar in_ >>= hPutChar out 
                end in_ out rout
              




    

