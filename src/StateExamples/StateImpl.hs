{-# LANGUAGE InstanceSigs #-}
module StateExamples.StateImpl where

newtype Phase s a =
    Phase { runPhase :: s -> (a, s)}

instance Functor (Phase s) where
    fmap :: (a -> b) -> Phase s a -> Phase s b
    fmap f (Phase g) = Phase $ \s -> let (a, s') = g s
                                     in (f a, s')

instance Applicative (Phase s) where
    pure x = Phase $ \s -> (x, s)

    (<*>) :: Phase s (a -> b) -> Phase s a -> Phase s b
    (Phase f) <*> (Phase g) = Phase $ 
                                \s -> let (h, s') = f s
                                          (a, s'') = g s'
                                      in (h a, s'')

instance Monad (Phase s) where
    return = pure

    (>>=) :: Phase s a -> (a -> Phase s b) -> Phase s b
    (Phase f) >>= g = Phase $
                        \s -> let (a, s') = f s
                                  Phase h = g a
                              in h s'