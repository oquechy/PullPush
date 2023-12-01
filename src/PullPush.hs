module PullPush (Pull, Push, pair, split, unpair, append) where

-- Pull

data Pull a = Pull Int (Int -> a)

instance Functor Pull where
  fmap f (Pull l ixf) = Pull l (f . ixf)

pair :: Pull a -> Pull (a,a)
pair (Pull l ixf) = Pull (l `div` 2) (\i -> (ixf (2*i), ixf (2*i + 1)))

split :: Pull a -> (Pull a, Pull a)
split (Pull l ixf) = (Pull l1 ixf, Pull l2 ixf')
    where l1 = l `div` 2
          l2 = l - l1
          ixf' i = ixf (i-l1)

-- Push

data Push a = Push Int ((Int -> a -> IO ()) -> IO ())

instance Functor Push where
  fmap f (Push l ixf) = Push l ixf'
    where ixf' write = ixf (\i a -> write i (f a))

unpair :: Push (a,a) -> Push a
unpair (Push l ixf) = Push (2*l) ixf'
    where ixf' write = ixf $ \i (a,b) -> do
            write (2*i) a
            write (2*i + 1) b

append :: Push a -> Push a -> Push a
append (Push l1 ixf1) (Push l2 ixf2) = Push (l1+l2) ixf
    where ixf write = do
            ixf1 write
            ixf2 (\i a -> write (i-l1) a)