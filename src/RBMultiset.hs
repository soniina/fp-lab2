module RBMultiset where

data Color = Red | Black deriving (Show, Eq)

data RBNode a
  = Empty
  | Node Color a Int (RBNode a) (RBNode a)
  deriving (Show)
