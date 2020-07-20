{-# LANGUAGE InstanceSigs #-}
module MonadTrans.MaybeT where

import Control.Applicative

newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
    fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
    fmap f (MaybeT a) = MaybeT $ (fmap . fmap) f a

instance Applicative m => Applicative (MaybeT m) where
    pure :: a -> MaybeT m a
    pure x = MaybeT $ (pure . pure) x

    (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
    (MaybeT fab) <*> (MaybeT mma) =
        MaybeT $ (<*>) <$> fab <*> mma

instance Monad m => Monad (MaybeT m) where
    return = pure

    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    (MaybeT ma) >>= k = 
        MaybeT $ do
            v <- ma
            case v of
                Nothing -> return Nothing
                (Just x) -> runMaybeT $ k x