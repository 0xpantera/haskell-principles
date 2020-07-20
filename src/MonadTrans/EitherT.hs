{-# LANGUAGE InstanceSigs #-}
module MonadTrans.EitherT where


newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
    fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

instance Applicative m => Applicative (EitherT e m) where
    pure :: a -> EitherT e m a
    pure x = EitherT $ (pure . pure) x

    (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
    (EitherT fab) <*> (EitherT mea) = 
        EitherT $ (<*>) <$> fab <*> mea

instance Monad m => Monad (EitherT e m) where
    return = pure

    (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    (EitherT ma) >>= k =
        EitherT $ do
            v <- ma
            case v of
                Left err -> return $ Left err
                Right x -> runEitherT $ k x
