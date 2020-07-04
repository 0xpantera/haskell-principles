{-# LANGUAGE InstanceSigs #-}
module MonadExamples.EitherMonad where

import Data.Functor
import Control.Applicative

type Founded = Int

type Coders = Int

data SoftwareShop =
    Shop {
        founded     :: Founded
      , programmers ::Coders
    } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
    | n < 0     = Left $ NegativeYears n
    | n > 500   = Left $ TooManyYears n
    | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
    | n < 0     = Left $ NegativeCoders n
    | n > 5000  = Left $ TooManyCoders n
    | otherwise = Right n

mkSoftware years coders = do
    founded <- validateFounded years
    programmers <- validateCoders coders

    if programmers > div founded 10
        then Left $
                TooManyCodersForYears
                founded programmers
        else Right $ Shop founded programmers


-- Implement the Either Monad

data Sum a b = First a | Second b
    deriving (Eq, Show)

instance Functor (Sum l) where
    fmap :: (a -> b) -> Sum l a -> Sum l b
    fmap _ (First x) = First x
    fmap f (Second y) = Second (f y)

instance Applicative (Sum l) where
    pure :: a -> Sum l a
    pure = Second

    (<*>) :: Sum l (a -> b) -> Sum l a -> Sum l b
    (Second f) <*> x = fmap f x
    (First f)  <*> _ = First f

instance Monad (Sum l) where
    return = pure

    (>>=) :: Sum l a -> (a -> Sum l b) -> Sum l b
    (First x) >>= _ = First x
    (Second y) >>= k = k y
