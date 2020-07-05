module MonadExamples.MonadComp where

import Control.Monad

mcomp :: Monad m =>
         (b -> m c)
      -> (a -> m b)
      -> a -> m c
mcomp f g x = join $ f <$> g x

mcomp' :: Monad m =>
          (b -> m c)
       -> (a -> m b)
       -> a -> m c
mcomp' f g x = g x >>= f

mcomp'' :: Monad m =>
           (b -> m c)
        -> (a -> m b)
        -> a -> m c
mcomp'' f g = g >=> f


sayHi :: String -> IO String
sayHi greeting = do
    putStrLn greeting
    getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = do
    getAge "Hello! How old are you? "