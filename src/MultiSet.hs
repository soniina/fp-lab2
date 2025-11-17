module MultiSet (MultiSet (..)) where

class (Foldable bag) => MultiSet bag where
  empty :: (Ord a) => bag a
  insert :: (Ord a) => a -> bag a -> bag a
  delete :: (Ord a) => a -> bag a -> bag a
  map :: (Ord b) => (a -> b) -> bag a -> bag b
  filter :: (Ord a) => (a -> Bool) -> bag a -> bag a
  toList :: bag a -> [a]
  fromList :: (Ord a) => [a] -> bag a
