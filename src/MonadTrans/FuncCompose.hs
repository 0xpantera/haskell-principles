{-# LANGUAGE InstanceSigs #-}
module MonadTrans.FuncCompose where

import Control.Applicative

newtype Identity a =
    Identity { runIdentity :: a }

newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
    fmap f (Compose fga) =
        Compose $ (fmap . fmap) f fga

newtype One f a =
    One (f a)
    deriving (Eq, Show)

instance Functor f => Functor (One f) where
    fmap g (One x) = One $ fmap g x

newtype Three f g h a =
    Three (f (g (h a)))

instance (Functor f, Functor g, Functor h) =>
         Functor (Three f g h) where
    fmap f (Three fgha) = 
        Three $ (fmap . fmap . fmap) f fgha

v :: Compose []
             Maybe
             (Compose Maybe [] Integer)
v = Compose [Just (Compose $ Just [1])]

instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure x = Compose $ (pure . pure) x

    (<*>) :: Compose f g (a -> b) 
          -> Compose f g a 
          -> Compose f g b
    (Compose fgab) <*> (Compose fga) = 
        Compose $ liftA2 (<*>) fgab fga

    liftA2 :: (a -> b -> c) 
           -> Compose f g a 
           -> Compose f g b 
           -> Compose f g c
    liftA2 f (Compose fga) (Compose fgb) = 
        Compose $ (liftA2 . liftA2) f fga fgb