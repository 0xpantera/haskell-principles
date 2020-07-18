{-# LANGUAGE InstanceSigs #-}
module MonadTrans.IdentityT where
    
-- from this
y :: Maybe Integer
y = fmap (+1) (Just 1)

-- to this
x :: Maybe (Integer, [Char], [Integer])
x = (,,) <$> Just 1 <*> Just "lol" <*> Just [1,2]

newtype Identity a =
    Identity { runIdentity :: a }

newtype IdentityT f a =
    IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity x) = Identity (f x)

instance Functor m => Functor (IdentityT m) where
    fmap :: (a -> b) -> IdentityT m a -> IdentityT m b
    fmap f (IdentityT fa) = IdentityT $ f <$> fa

instance Applicative Identity where
    pure :: a -> Identity a
    pure x = Identity x

    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    (Identity f) <*> (Identity x) = Identity $ f x

instance Applicative m => Applicative (IdentityT m) where
    pure :: a -> IdentityT m a
    pure x = IdentityT $ pure x

    (<*>) :: IdentityT m (a -> b) -> IdentityT m a -> IdentityT m b
    (IdentityT f) <*> (IdentityT ma) = IdentityT $ f <*> ma

instance Monad Identity where
    return = pure

    (>>=) :: Identity a -> (a -> Identity b) -> Identity b
    (Identity x) >>= k = k x

instance Monad m => Monad (IdentityT m) where
    return = pure

    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (IdentityT ma) >>= k = IdentityT $ ma >>= runIdentityT . k

-- aimb :: (a -> IdentityT m b) -> m a -> m (IdentityT m b) 
-- aimb = fmap f ma

-- aimb2 :: (IdentityT m a -> m a) -> m (IdentityT m a) -> m (m a)
-- aimb2 = fmap runIdentityT aimb

-- aimb3 :: m (m a) -> m a
-- aimb3 = join aimb2

-- aimb4 :: m a -> IdentityT m a
-- aimb4 = IdentityT aimb3