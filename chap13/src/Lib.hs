{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
    
module Lib
    ( 
    ) where

import Prelude hiding (take, FilePath)
import qualified Data.Map as M
import Control.Monad ( (>=>), (<=<))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import qualified System.IO as IO
import Control.Exception (catch)
import Control.Monad.Except (MonadError)

-- tic tac toe

data Coordonate = One | Two | Three deriving (Ord, Eq)
data Player = X | O deriving (Ord, Eq)
data Position = Position {
    x :: Coordonate, 
    y :: Coordonate
} deriving (Ord, Eq, Num)
data Result = AlreadyTaken { by :: Player } 
            | NextTurn 
            | GameEnded { winner :: Player }


class TicTacToe m where 
    info :: Position -> m (Maybe Player)
    take :: Position -> m Result

type Board = M.Map Position Player

takeIfNotTaken :: (Monad m, TicTacToe m) => Position -> m (Maybe Result)
takeIfNotTaken position =  do 
    player <- info position 
    case player of 
        Nothing ->  Just <$> take position
        Just  _ ->  return Nothing

instance TicTacToe (ReaderT Player (StateT Board IO)) where 
    info position = M.lookup position <$> lift get 
    take position = do 
      player <- info position
      case player of 
        Just p  -> return AlreadyTaken { by = p }
        Nothing -> do 
            currentPlayer <- ask
            lift $ modify (M.insert position currentPlayer) 
            return NextTurn


logic :: (Monad m, TicTacToe m) => m Result
logic = undefined

runTicTacToe :: IO (Result, Board)
runTicTacToe = runStateT (runReaderT logic X) M.empty


-- Mock FileSystem 

type FilePath = String
type FSError  = IOError

class FS m where
    writeFile :: FilePath -> String -> m (Either FSError ())
    readFile  :: FilePath -> m (Either FSError String) 


instance FS IO where 
    writeFile path content =  (Right <$> IO.writeFile path content) `catch`  (\e -> return (Left e))
    readFile  path = (Right <$> IO.readFile path) `catch` (\e -> return (Left e))

type MockFileSystem =  M.Map FilePath String

instance FS  (State MockFileSystem) where 
    writeFile path content = Right <$> (modify $ M.insert path content )
    readFile  path = do 
        fs <- get 
        pure $ case M.lookup path fs of 
            Nothing -> Left (userError "path do no exists")
            Just c  -> Right c


-- tic tac toe as Monad 

data  TicTacToe' a =   Info Position (Maybe Player -> TicTacToe' a)
                |   Take Position (Result -> TicTacToe' a) 
                |   Done a

    

instance Monad TicTacToe' where
  Done x   >>= f = f x
  Info p g >>= f = Info p (g >=> f)
  Take p g >>= f = Take p (g >=> f)


instance Functor TicTacToe' where
  fmap f (Done x    ) = Done (f x)
  fmap f (Info pos g) = Info pos (fmap f . g)
  fmap f (Take pos g) = Take pos (fmap f . g)

instance Applicative TicTacToe' where
  pure = Done
  Done f     <*> Done x      = Done $ f x
  Info pos f <*> Info pos' x = Info
    (pos + pos')
    (\y ->
      let f' = f y
          x' = x y
      in  f' <*> x'
    )
  Take pos f <*> Take pos' x = Take
    (pos + pos')
    (\y ->
      let f' = f y
          x' = x y
      in  f' <*> x'
    )


info' :: Position -> TicTacToe' (Maybe Player)
info' p = Info p $ (\pl -> Done pl)

take' :: Position -> TicTacToe' (Result)
take' p = Take p $ (\r -> Done r)

takeIfNotTaken' :: Position -> TicTacToe' (Maybe Result)
takeIfNotTaken' p = Info p $ \i -> 
        case i of 
            Just _   -> Done Nothing
            Nothing  -> Take p $ (\r -> Done (Just r))

interpertTicTacToe' :: TicTacToe' a -> ReaderT Player (StateT Board IO) a
interpertTicTacToe' (Done a) = return a
interpertTicTacToe' (Info p cont) = do 
           pl <- M.lookup p <$> (lift $ get)
           interpertTicTacToe' (cont pl)
interpertTicTacToe' (Take p cont) = do 
            mpl <- M.lookup p <$> (lift $ get)
            case mpl of 
                Just pl -> interpertTicTacToe' (cont (AlreadyTaken pl))
                Nothing -> do 
                    currentPlayer <- ask
                    lift $ modify (M.insert p currentPlayer) 
                    interpertTicTacToe' (cont NextTurn)

data FS' a = 
        WriteFile FilePath  String  (Either FSError () -> FS' a)
    |   ReadFile  FilePath (Either FSError String -> FS' a)
    |   FSDone    a

writeFile' :: FilePath -> String -> FS' (Either FSError ())
writeFile' fp contents = WriteFile fp  contents FSDone 

readFile' :: FilePath  -> FS' (Either FSError String)
readFile' fp  = ReadFile fp  FSDone 

interpertFS' :: FS' a -> IO a 
interpertFS' (FSDone a) = return a
interpertFS' (WriteFile fp contents cont) = do 
        IO.writeFile fp contents 
        interpertFS' (cont (Right()))
    `catch` \e -> interpertFS' (cont (Left e))
interpertFS' (ReadFile fp cont) = do 
        content <- IO.readFile fp 
        interpertFS' (cont  (Right(content)))
    `catch` \e -> interpertFS' (cont (Left e))


interpertMockFS' :: FS' a -> State MockFileSystem  a
interpertMockFS' (FSDone a) = return a 
interpertMockFS' (WriteFile fp content cont) = do 
          modify (M.insert fp content)
          interpertMockFS' (cont (Right()))
interpertMockFS' (ReadFile fp cont) = do
          maybeContent <- M.lookup fp <$> get 
          case maybeContent of
            Just content -> interpertMockFS' (cont (Right $ content))
            Nothing      -> interpertMockFS' (cont (Left $ userError "file no found"))

data TicTacToeF r =  
          InfoF Position (Maybe Player  -> r )
        | TakeF Position ( Result -> r )

instance Functor TicTacToeF where
    fmap f (InfoF pos cont) = InfoF pos $ (\mplayer -> f $ cont $ mplayer)
    fmap f (TakeF pos cont) = TakeF pos $ (\r -> f $ cont $ r)

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


type TicTacToe'' = Free TicTacToeF

liftF :: Functor f => f a -> Free f a
liftF = Free . fmap return

info'' :: Position -> Free TicTacToeF (Maybe Player)
info'' p = liftF (InfoF p id)

data FSF r = 
        WriteFile'' FilePath String (Either FSError () -> r)
    |   ReadFile''  FilePath (Either FSError String -> r)

instance Functor FSF where
    fmap f (WriteFile'' path content cont) =  WriteFile'' path  content  ( f . cont)
    fmap f (ReadFile'' path cont) = ReadFile'' path (f .cont)

type FS'' = Free FSF 

interpertFSF :: FSF a -> IO a 
interpertFSF (WriteFile'' path content cont) = do 
            IO.writeFile path content 
            return $ cont $ Right()
        `catch` (\e -> return $ cont $ Left e)
interpertFSF (ReadFile'' path cont) = do 
            content <- IO.readFile path  
            return $ cont $ Right(content)
        `catch` (\e -> return $ cont $ Left e)

foldFree :: Monad m => (forall r . f r -> m r) -> Free f a -> m a
foldFree _         (Pure x) = return x
foldFree interpret (Free x) = do
  x' <- interpret x
  foldFree interpret x'

interpertFS'' :: FS'' a -> IO a
interpertFS'' = foldFree interpertFSF 


data TicTacToe''' a where
    Info'''  :: Position -> TicTacToe''' (Maybe Player)
    Take'''  :: Position -> TicTacToe''' (Result)
    Bind     :: TicTacToe''' a -> (a -> TicTacToe''' b) -> TicTacToe''' b 
    Return   ::  a -> TicTacToe''' a


instance Functor TicTacToe''' where
    fmap f x = Bind x (Return . f)

instance Applicative TicTacToe''' where 
    pure = Return 
    f <*> x = f `Bind` (\f'-> x `Bind` (\x' -> Return (f' x')) )

instance Monad TicTacToe''' where 
    return = Return
    (>>=)  = Bind

info''' :: Position -> TicTacToe''' (Maybe Player)
info''' = Info''' 

take''' :: Position -> TicTacToe''' (Result)
take''' = Take''' 

takeIfNotTaken''' :: Position -> TicTacToe''' (Maybe Result)
takeIfNotTaken''' position = do 
      mplayer <- info''' position
      case mplayer of 
         Just player -> return  Nothing
         Nothing     ->  Just <$> (Take''' position)


runGame''' :: TicTacToe''' a -> ReaderT Player (StateT Board IO) a
runGame''' (Return a)  = return a
runGame''' (Bind x f)  = runGame''' x >>= runGame''' . f 
runGame''' (Info''' p) = M.lookup p <$>  lift get 
runGame''' (Take''' p) = do 
    mplayer <- M.lookup p <$>  lift get 
    case mplayer of 
        Just player -> return (AlreadyTaken player)
        Nothing     -> do 
            me <- ask
            lift (modify (M.insert p me))
            return NextTurn


data FS'''  a where 
    WriteFile''' :: FilePath -> String -> FS''' (Either FSError ())
    ReadFile'''  :: FilePath -> FS''' (Either FSError String)
    BindFS       :: FS''' a  -> (a -> FS''' b) -> FS''' b
    ReturnFS     :: a -> FS''' a 


-- program package 

data TicTacToeI a where 
    InfoI :: Position -> TicTacToeI (Maybe Player)
    TakeI :: Position -> TicTacToeI Result

data Program inst a where
    DoneP :: a -> Program inst a 
    BindP :: Program inst a  -> (a -> Program inst b) -> Program inst b
    Inst  :: inst a -> Program inst a

instance Functor (Program instr) where
  fmap f x = undefined 

instance Applicative (Program instr) where
  pure = DoneP
  f <*> x = undefined 

instance Monad (Program instr) where
  (>>=) = BindP

    

-- freer 

data Freer inst a where
    PureF :: a -> Freer inst a
    ImpureF :: inst a -> (a -> Freer inst b) -> Freer inst b 

instance Functor (Freer inst) where
    fmap f (PureF x) = PureF (f x)
    fmap f (ImpureF x k) = ImpureF x (fmap f . k)

instance Applicative (Freer inst) where 
    pure a = PureF a
    PureF f    <*> PureF x    = PureF $ f x
    ImpureF x k <*> f          = ImpureF x (\a -> k a <*> f)
    PureF f <*> ImpureF x k    = ImpureF x (fmap f . k)

instance Monad (Freer inst) where
    return a = PureF a
    (PureF a)      >>= f = f a 
    (ImpureF x k)  >>= f = ImpureF x (f <=< k)

-- rpn free

data IStackF r = Push Integer r | Pop (Integer -> r ) deriving (Functor)
type IStack = Free IStackF

pop :: IStack Integer
pop = liftF (Pop id)

push :: Integer -> IStack ()
push  i = liftF (Push i ())

data RpnInstruction = Number Integer | Plus | Times 


evaluate :: [RpnInstruction] -> IStack Integer
evaluate [] = pop
evaluate ((Number n) : xs) = push n >> evaluate xs
evaluate (Plus : xs) =  ((+) <$> pop <*> pop) >>= push >> evaluate xs 
evaluate (Times : xs) =  ((*) <$> pop <*> pop) >>= push >> evaluate xs 

interpertIStackF :: IStackF r -> State [Integer] r
interpertIStackF (Push n r) = do 
    modify (\xs -> (n:xs))
    pure r

interpertIStackF (Pop f) = do 
    xs <- get 
    let (s, x) = pop xs
    put s
    pure $ f x
    where
      pop :: [a] -> ([a], a)
      pop [] = error "Two values must be on the stack to call operations"
      pop xs = (init xs, last xs)

interpertIStack = foldFree interpertIStackF

evaluateRPN :: [RpnInstruction] -> Maybe Integer
evaluateRPN instruction = 
    let (x, s)  = runState (interpertIStack $  evaluate $ instruction) []
    in if null s then Just x else Nothing


-- free applicative functor 

data Ap f a where 
    PureAp :: a -> Ap f a
    Ap     :: f a -> Ap f (a -> b) -> Ap f b  

instance (Functor f) => Functor (Ap f) where
    fmap g (PureAp a) = PureAp (g a)
    fmap g (Ap x y)   = Ap x (fmap (g .) y)

instance (Functor f) => Applicative (Ap f) where
    pure            = PureAp
    PureAp f <*> x  = fmap f x 
    Ap x y   <*> z  = Ap x (flip  <$> y <*> z)


data Arg a = 
      Flag String (Bool -> a)
    | Option String (Maybe String -> a)
    deriving (Functor)

type CommandLine = Ap Arg

flag :: String -> CommandLine Bool
flag x = Ap (Flag x id) (PureAp id)

option :: String -> CommandLine (Maybe String)
option x = Ap (Option x id) (PureAp id)


data Config = Config { background :: Bool, file :: String }

readCommandLine :: CommandLine Config 
readCommandLine = Config <$> flag "backgroung" <*> (maybe "out" id <$> option "file")


argNames :: CommandLine a -> [String]
argNames (PureAp _) = []
argNames (Ap arg rest) = [name arg] ++ argNames rest 
    where 
        name :: Arg a -> String
        name (Flag s _)      = s
        name (Option s _)    = s   




