{-# LANGUAGE InstanceSigs #-}
module MonadTrans.StateT where

import Data.Bifunctor

newtype StateT s m a =
    StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    fmap f (StateT sma) = StateT $ 
                            \s -> 
                                let r = sma s
                                in first f <$> r

instance Monad m => Applicative (StateT s m) where
    pure :: a -> StateT s m a
    pure x = StateT $ \s -> pure (x, s)

    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    (StateT mab) <*> (StateT sma) = StateT $ 
                                        \s -> do
                                            (a, s') <- sma s
                                            (ab, s'') <- mab s'
                                            return $ (ab a, s'')

instance Monad m => Monad (StateT s m) where
    return = pure

    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    (StateT sma) >>= k = StateT $
                            \s -> do
                                (a, s') <- sma s
                                runStateT (k a) s'

