{-# OPTIONS_GHC -Wno-partial-fields #-}

module RBMultiSet (RBMultiSet (..), validRBTree, getRBNode) where

import Data.Maybe (isJust)
import MultiSet (MultiSet (..))

newtype RBMultiSet a = RBMultiSet (RBNode a) deriving (Show)

instance MultiSet RBMultiSet where
  empty = mempty
  insert x (RBMultiSet tree) = RBMultiSet (insertRB x tree)
  delete x (RBMultiSet tree) = RBMultiSet (deleteRB x tree)
  map f (RBMultiSet tree) = RBMultiSet (fmap f tree)
  filter p (RBMultiSet tree) = RBMultiSet (filterRB p tree)
  toList (RBMultiSet tree) = toListRB tree
  fromList xs = RBMultiSet (fromListRB xs)

instance (Ord a) => Eq (RBMultiSet a) where
  (==) (RBMultiSet tree1) (RBMultiSet tree2) = areEqual (pushLefts tree1 []) (pushLefts tree2 [])
    where
      areEqual nodes1 nodes2 =
        case (nextElem nodes1, nextElem nodes2) of
          (Nothing, Nothing) -> True
          (Nothing, _) -> False
          (_, Nothing) -> False
          (Just ((x1, c1), nodes1'), Just ((x2, c2), nodes2')) ->
            x1 == x2 && c1 == c2 && areEqual nodes1' nodes2'

      pushLefts :: RBNode a -> [RBNode a] -> [RBNode a]
      pushLefts Nil nodes = nodes
      pushLefts node@(Node {left = l}) nodes = pushLefts l (node : nodes)

      nextElem :: [RBNode a] -> Maybe ((a, Int), [RBNode a])
      nextElem [] = Nothing
      nextElem (Node {value = x, count = n, right = r} : nodes) = Just ((x, n), pushLefts r nodes)
      nextElem (Nil : _) = error "unexpected Nil in nodes"

instance (Ord a) => Semigroup (RBMultiSet a) where
  (RBMultiSet a) <> (RBMultiSet b) = RBMultiSet (unionRB a b)

instance (Ord a) => Monoid (RBMultiSet a) where
  mempty = RBMultiSet Nil

instance Foldable RBMultiSet where
  foldMap f (RBMultiSet node) = foldMap f node

data Color = Red | Black deriving (Show, Eq)

data RBNode a
  = Nil
  | Node
      { color :: Color,
        value :: a,
        count :: Int,
        left :: RBNode a,
        right :: RBNode a
      }
  deriving (Show)

instance Functor RBNode where
  fmap _ Nil = Nil
  fmap f node@(Node {value = x, left = l, right = r}) = node {value = f x, left = fmap f l, right = fmap f r}

instance Foldable RBNode where
  foldMap _ Nil = mempty
  foldMap f (Node {value = x, count = n, left = l, right = r}) = foldMap f l <> mconcat (replicate n (f x)) <> foldMap f r

unionRB :: (Ord a) => RBNode a -> RBNode a -> RBNode a
unionRB Nil t = t
unionRB t Nil = t
unionRB t1 t2 = foldr insertRB t2 (toListRB t1)

insertRB :: (Ord a) => a -> RBNode a -> RBNode a
insertRB x tree = makeBlack (ins tree)
  where
    ins Nil = Node Red x 1 Nil Nil
    ins node@(Node {value = y, count = n, left = l, right = r})
      | x < y = balance node {left = ins l}
      | x > y = balance node {right = ins r}
      | otherwise = node {count = n + 1}

    makeBlack Nil = Nil
    makeBlack node = node {color = Black}

findCountAndReduce :: (Ord a) => a -> RBNode a -> (RBNode a, Bool)
findCountAndReduce _ Nil = (Nil, False)
findCountAndReduce x node@(Node {value = y, count = n, left = l, right = r})
  | x < y =
      let (newLeft, deleted) = findCountAndReduce x l
       in (node {left = newLeft}, deleted)
  | x > y =
      let (newRight, deleted) = findCountAndReduce x r
       in (node {right = newRight}, deleted)
  | n > 1 = (node {count = n - 1}, False)
  | otherwise = (node, True)

deleteRB :: (Ord a) => a -> RBNode a -> RBNode a
deleteRB val tree = if deleted then makeBlack $ del val tree else newTree
  where
    (newTree, deleted) = findCountAndReduce val tree

    makeBlack Nil = Nil
    makeBlack node = node {color = Black}

    del :: (Ord a) => a -> RBNode a -> RBNode a
    del x node@(Node {value = y, left = l, right = r})
      | x < y = delL x node
      | x > y = delR x node
      | otherwise = fuse l r
    del _ Nil = Nil

    delL :: (Ord a) => a -> RBNode a -> RBNode a
    delL x node@(Node {color = Black, left = l}) = balL $ node {left = del x l}
    delL x node@(Node {color = Red, left = l}) = node {left = del x l}
    delL _ Nil = Nil

    balL :: RBNode a -> RBNode a
    balL node@(Node {color = Black, left = childNode@(Node {color = Red})}) = node {color = Red, left = childNode {color = Black}}
    balL node@(Node {color = Black, right = childNode@(Node {color = Black})}) = balance node {right = childNode {color = Red}}
    balL (Node Black gv gc gl (Node Red pv pc (Node Black sv sc sl sr) (Node Black cv cc cl cr))) =
      Node Red sv sc (Node Black gv gc gl sl) (balance (Node Black pv pc sr (Node Red cv cc cl cr)))
    balL node = node

    delR :: (Ord a) => a -> RBNode a -> RBNode a
    delR x node@(Node {color = Black, right = r}) = balR $ node {right = del x r}
    delR x node@(Node {color = Red, right = r}) = node {right = del x r}
    delR _ Nil = Nil

    balR :: RBNode a -> RBNode a
    balR node@(Node {color = Black, right = childNode@(Node {color = Red})}) = node {color = Red, right = childNode {color = Black}}
    balR node@(Node {color = Black, left = childNode@(Node {color = Black})}) = balance node {left = childNode {color = Red}}
    balR (Node Black gv gc (Node Red pv pc (Node Black cv cc cl cr) (Node Black sv sc sl sr)) gr) =
      Node Red sv sc (balance (Node Black pv pc (Node Red cv cc cl cr) sl)) (Node Black gv gc sr gr)
    balR node = node

    fuse :: RBNode a -> RBNode a -> RBNode a
    fuse Nil r = r
    fuse l Nil = l
    fuse tree1@(Node {color = Black}) tree2@(Node {color = Red, left = l2}) = tree2 {left = fuse tree1 l2}
    fuse tree1@(Node {color = Red, right = r1}) tree2@(Node {color = Black}) = tree1 {right = fuse r1 tree2}
    fuse tree1@(Node {color = Red, right = r1}) tree2@(Node {color = Red, left = l2}) =
      let s = fuse r1 l2
       in case s of
            node@(Node {color = Red, left = sl, right = sr}) -> node {left = tree1 {right = sl}, right = tree2 {left = sr}}
            _ -> tree1 {right = tree2 {left = s}}
    fuse tree1@(Node {color = Black, right = r1}) tree2@(Node {color = Black, left = l2}) =
      let s = fuse r1 l2
       in case s of
            node@(Node {color = Red, left = sl, right = sr}) -> node {left = tree1 {right = sl}, right = tree2 {left = sr}}
            _ -> balL (tree1 {right = tree2 {left = s}})

balance :: RBNode a -> RBNode a
balance (Node Black gv gc (Node Red pv pc (Node Red xv xc xl xr) pr) gr) =
  Node Red pv pc (Node Black xv xc xl xr) (Node Black gv gc pr gr)
balance (Node Black gv gc (Node Red xv xc xl (Node Red pv pc pl pr)) gr) =
  Node Red pv pc (Node Black xv xc xl pl) (Node Black gv gc pr gr)
balance (Node Black gv gc gl (Node Red pv pc pr (Node Red xv xc xl xr))) =
  Node Red pv pc (Node Black gv gc gl pr) (Node Black xv xc xl xr)
balance (Node Black gv gc gl (Node Red xv xc (Node Red pv pc pl pr) xr)) =
  Node Red pv pc (Node Black gv gc gl pl) (Node Black xv xc pr xr)
balance node = node

toListRB :: RBNode a -> [a]
toListRB Nil = []
toListRB (Node {value = x, count = n, left = l, right = r}) = toListRB l ++ replicate n x ++ toListRB r

fromListRB :: (Ord a) => [a] -> RBNode a
fromListRB = foldr insertRB Nil

filterRB :: (Ord a) => (a -> Bool) -> RBNode a -> RBNode a
filterRB _ Nil = Nil
filterRB p tree = fromListRB (Prelude.filter p (toListRB tree))

getRBNode :: RBMultiSet a -> RBNode a
getRBNode (RBMultiSet node) = node

blackPathCount :: RBNode a -> Maybe Int
blackPathCount Nil = Just 1
blackPathCount (Node {color = c, left = l, right = r}) = do
  leftCount <- blackPathCount l
  rightCount <- blackPathCount r
  if leftCount /= rightCount
    then Nothing
    else Just (leftCount + if c == Black then 1 else 0)

noRedChildrenForRed :: RBNode a -> Bool
noRedChildrenForRed Nil = True
noRedChildrenForRed node@Node {left = l, right = r} = not (isRed node && (isRed l || isRed r)) && noRedChildrenForRed l && noRedChildrenForRed r

isRed :: RBNode a -> Bool
isRed Node {color = Red} = True
isRed _ = False

rootBlack :: RBNode a -> Bool
rootBlack (Node {color = Red}) = False
rootBlack _ = True

validRBTree :: RBMultiSet a -> Bool
validRBTree multiset = rootBlack tree && noRedChildrenForRed tree && isJust (blackPathCount tree)
  where
    tree = getRBNode multiset
