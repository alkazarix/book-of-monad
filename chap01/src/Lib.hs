module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Tree a = Leaf a | Node (Tree a) (Tree a)


numberOfLeaves :: Tree a -> Integer
numberOfLeaves (Leaf _) = 1
numberOfLeaves (Node l r) = numberOfLeaves l + numberOfLeaves r + 1


relabel :: Tree a -> Integer -> (Tree (a, Integer), Integer)
relabel (Leaf x) i = (Leaf (x, i), i)
relabel (Node l r) i = let (l', i') = relabel l i
                           (r', i'') = relabel r i'
                        in (Node l' r', i'')  

type WithCounter a =  Integer -> (a, Integer)  


next :: WithCounter a -> (a -> WithCounter b) -> WithCounter b 
f `next` g = \i -> let (a, i') = f i in g a i' 

withCounter :: a -> WithCounter a 
withCounter x = \i -> (x, i)    


relabel' :: Tree a -> Integer -> (Tree (a, Integer), Integer)     
relabel' (Leaf x) = \i -> (Leaf (x, i), i) 
relabel' (Node l r) = relabel' l `next` \l' -> 
                         relabel' r `next` \r' -> 
                         withCounter (Node l' r') 

runRelabel :: Tree a -> Tree (a, Integer) 
runRelabel t = fst $ (relabel' t 0) 


type State s a = s ->  (a, s)   

bind :: State s a -> (a -> State s b) -> State s b 
bind f g = \s -> let (a, s') = f s in g a s'  

pure :: a -> State s a 
pure a = \s -> (a, s)                                                                               


len :: [a] -> Integer
len []    = 0
len (_:t) = 1 + len t


concat' :: [a] -> [a] -> [a]
concat' [] xs    = xs
concat' (h:t) xs = h :  (t `concat'` xs)

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x) :  f `map'` xs

data Person =
  Person
    { name :: String
    , age  :: Integer
    }

validateName :: String -> Maybe String
validateName = undefined

validateAge :: Integer -> Maybe Integer
validateAge = undefined

validatePerson :: String -> Integer -> Maybe Person
validatePerson name age = case validateName name of   
    Nothing  -> Nothing 
    Just name' -> 
        case validateAge age of 
            Nothing -> Nothing 
            Just age' -> Just (Person name' age') 

then' :: Maybe a -> (a -> Maybe b) -> Maybe b 
then' a f = case a of 
    Nothing -> Nothing 
    Just a' -> f a' 

validatePerson' :: String -> Integer -> Maybe Person
validatePerson' name age = validateName name `then'` \name' -> 
    validateAge age `then'` \age' -> Just (Person name' age')


