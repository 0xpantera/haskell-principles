module MonadExamples.Exercises where

import Control.Monad (join)
import Control.Applicative ((*>))

-- A flipped version of (>>=)
bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join (fmap f x)


sequencing :: IO ()
sequencing = do
    putStrLn "blah"
    putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
    putStrLn "blah" >>
    putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
    putStrLn "blah" *>
    putStrLn "another thing"

binding :: IO ()
binding = do
    name <- getLine
    putStrLn name

binding' :: IO ()
binding' = 
    getLine >>= putStrLn

bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn $ "y helo thar: " <> name

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
    putStrLn "name pls:" >>
    getLine >>= 
        \name -> 
            putStrLn $ "y helo thar: " <> name

twoBinds :: IO ()
twoBinds = do
    putStrLn "name pls:"
    name <- getLine

    putStrLn "age pls:"
    age <- getLine

    putStrLn $ "y helo thar:"
            <> name <> " who is:"
            <> age <> " years old."

twoBinds' :: IO ()
twoBinds' =
    putStrLn "name pls:" >>
    getLine >>=
        \name -> putStrLn "age pls:" >>
        getLine >>= \age ->
            putStrLn $ "y helo thar:"
                     <> name <> " who is:"
                     <> age <> " years old."

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else [x*x]