instance Functor MaybeNull where
    fmap f Null = Null
    fmap f (JustVal a) = JustVal (f a)