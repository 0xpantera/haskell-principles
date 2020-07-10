{-# LANGUAGE InstanceSigs #-}
module ReaderExamples.WarmUp where

import Control.Applicative
import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> id <*> composed

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
    og <- id
    res <- fmapped
    return (og, res)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = 
    id >>= 
        \og -> fmapped >>= 
            \res -> return (og, res)


newtype Reader r a =
    Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader $ f

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure x = Reader $ const x

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
    return = pure

    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r