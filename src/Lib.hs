module Lib
    ( Reader(..)
      , ask
      , asks
      , Environ(..)
      , xMultY
      , xyz
      , ReaderT(..)
      , listedXMultY
    ) where

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader $ \i -> f (g i)

instance Applicative (Reader r) where
  pure x = Reader $ \i -> x
  (Reader g) <*> (Reader h) = Reader $ \i ->
    let f = g i
        x = h i
    in f x

instance Monad (Reader r) where
  (Reader g) >>= f = Reader $ \i ->
    let a = g i
        h = runReader $ f a
        b = h i
    in b

----------------------------------

ask :: Reader r r
ask = Reader $ \i -> i

asks :: (a -> b) -> Reader a b
asks f = Reader $ \i -> f i

----------------------------------

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Monad m => Functor (ReaderT r m) where
  fmap f (ReaderT g) = ReaderT $ \i -> do
    a <- g i
    return $ f a
    -- Alternative, \i -> fmap f (g i)
    -- ?

instance Monad m => Applicative (ReaderT r m) where
  pure x = ReaderT $ \i -> return x
  (ReaderT g) <*> (ReaderT h) = ReaderT $ \i -> do
    f <- g i
    x <- h i
    return $ f x

instance Monad m => Monad (ReaderT r m) where
  (ReaderT g) >>= f = ReaderT $ \i -> do
    a1 <- g i
    let h = runReaderT (f a1)
    a2 <- h i
    return $ a2

----------------------------------

tAsk :: Monad m => ReaderT r m r
tAsk = ReaderT $ \i -> return i

tAsks :: Monad m => (a -> b) -> ReaderT a m b
tAsks f = ReaderT $ \i -> return (f i)

tAsksM :: (a -> m b) -> ReaderT a m b
tAsksM f = ReaderT $ \i -> f i

----------------------------------

data Environ = Enviro { getX :: Int, getY :: Int, getZ :: Int }

xMultY :: Reader Environ Int
xMultY = do
  x <- asks getX
  y <- asks getY
  return $ x * y

xyz :: Reader Environ Int
xyz = do
  xy <- xMultY
  z <- asks getZ
  return $ xy * z

----------------------------------

listedXMultY :: ReaderT Environ [] Int
listedXMultY = do
  --x <- tAsksM (singletonList . getX)
  x <- tAsksM (pairWithIncrement . getX)
  y <- tAsksM (pairWithIncrement . getY)
  return $ x * y

----------------------------------

singletonList :: Int -> [Int]
singletonList x = [x]

pairWithIncrement :: Int -> [Int]
pairWithIncrement x = [x, x + 1]
