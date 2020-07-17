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

