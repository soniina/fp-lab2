{-# OPTIONS_GHC -Wno-partial-fields #-}

module RBMultiSet (RBMultiSet (..), validRBTree, getRBNode) where

import Data.Maybe (isJust)
import MultiSet (MultiSet (..))

newtype RBMultiSet a = RBMultiSet (RBNode a) deriving (Show)

instance MultiSet RBMultiSet where
  empty = mempty
  insert x (RBMultiSet tree) = RBMultiSet (insertRB x tree)
  delete x (RBMultiSet tree) = RBMultiSet (deleteRB x tree)
  map f (RBMultiSet tree) = fromList (toListRB (fmap f tree))
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

makeBlack :: RBNode a -> RBNode a
makeBlack Nil = Nil
makeBlack node = node {color = Black}

insertRB :: (Ord a) => a -> RBNode a -> RBNode a
insertRB x tree = makeBlack (ins tree)
  where
    ins Nil = Node Red x 1 Nil Nil
    ins node@(Node {value = y, count = n, left = l, right = r})
      | x < y = balance node {left = ins l}
      | x > y = balance node {right = ins r}
      | otherwise = node {count = n + 1}

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

deleteRB :: (Ord a) => a -> RBNode a -> RBNode a
deleteRB _ Nil = Nil
deleteRB x root =
  case tryReduce x root of
    Just newRoot -> newRoot
    Nothing -> makeBlack (fst (deleteValue x root))
  where
    tryReduce :: (Ord a) => a -> RBNode a -> Maybe (RBNode a)
    tryReduce _ Nil = Just Nil
    tryReduce val node@Node {value = v, left = l, right = r, count = c}
      | val < v =
          case tryReduce val l of
            Just l' -> Just (node {left = l'})
            Nothing -> Nothing
      | val > v =
          case tryReduce val r of
            Just r' -> Just (node {right = r'})
            Nothing -> Nothing
      | otherwise =
          if c > 1
            then Just (node {count = c - 1})
            else Nothing

    deleteValue :: (Ord a) => a -> RBNode a -> (RBNode a, Bool)
    deleteValue _ Nil = (Nil, False)
    deleteValue val node@Node {value = v, left = l, right = r}
      | val < v =
          let (l', shrunk) = deleteValue val l
           in if shrunk then balanceLeft node {left = l'} else (node {left = l'}, False)
      | val > v =
          let (r', shrunk) = deleteValue val r
           in if shrunk then balanceRight node {right = r'} else (node {right = r'}, False)
      | otherwise = removeNode node

    removeNode :: RBNode a -> (RBNode a, Bool)
    removeNode Node {left = Nil, right = Nil, color = col} = (Nil, col == Black)
    removeNode Node {left = child@Node {}, right = Nil, color = Black} = (makeBlack child, False)
    removeNode Node {left = Nil, right = child@Node {}, color = Black} = (makeBlack child, False)
    removeNode node@Node {right = r} =
      let (minVal, minCount) = getMin r
          (r', shrunk) = removeMin r
          newNode = node {value = minVal, count = minCount, right = r'}
       in if shrunk then balanceRight newNode else (newNode, False)
    removeNode _ = (Nil, False)

    getMin :: RBNode a -> (a, Int)
    getMin Node {left = Nil, value = v, count = c} = (v, c)
    getMin Node {left = l} = getMin l
    getMin Nil = error "unexpected Nil while getMin"

    removeMin :: RBNode a -> (RBNode a, Bool)
    removeMin node@Node {left = Nil} = removeNode node
    removeMin node@Node {left = l} =
      let (l', shrunk) = removeMin l
       in if shrunk then balanceLeft node {left = l'} else (node {left = l'}, False)
    removeMin Nil = (Nil, False)

    balanceLeft :: RBNode a -> (RBNode a, Bool)
    balanceLeft node@Node {color = Black, right = sib@Node {color = Red, left = sl}} =
      let (n', shrunk) = balanceLeft node {right = sl, color = Red}
       in (sib {left = n', color = Black}, shrunk)
    balanceLeft node@Node {color = col, right = sib@Node {color = Black, left = sl, right = sr}}
      | isBlack sl && isBlack sr =
          let newCol = case col of Red -> Black; Black -> Black
           in (node {color = newCol, right = sib {color = Red}}, col == Black)
      | isRed sl && isBlack sr =
          let sib' = sib {color = Red, left = makeBlack sl}
           in balanceLeft node {right = rotateRight sib'}
      | otherwise =
          (rotateLeft node {color = color sib, right = sib {color = col, right = makeBlack sr}}, False)
    balanceLeft n = (n, False)

    balanceRight :: RBNode a -> (RBNode a, Bool)
    balanceRight node@Node {color = Black, left = sib@Node {color = Red, right = sr}} =
      let (n', shrunk) = balanceRight node {left = sr, color = Red}
       in (sib {right = n', color = Black}, shrunk)
    balanceRight node@Node {color = col, left = sib@Node {color = Black, left = sl, right = sr}}
      | isBlack sl && isBlack sr =
          (node {color = Black, left = sib {color = Red}}, col == Black)
      | isBlack sl && isRed sr =
          let sib' = sib {color = Red, right = makeBlack sr}
           in balanceRight node {left = rotateLeft sib'}
      | otherwise =
          (rotateRight node {color = color sib, left = sib {color = col, left = makeBlack sl}}, False)
    balanceRight n = (n, False)

    rotateLeft :: RBNode a -> RBNode a
    rotateLeft node@Node {right = r@Node {left = b}} = r {left = node {right = b}}
    rotateLeft t = t

    rotateRight :: RBNode a -> RBNode a
    rotateRight node@Node {left = l@Node {right = b}} = l {right = node {left = b}}
    rotateRight t = t

toListRB :: RBNode a -> [a]
toListRB Nil = []
toListRB (Node {value = x, count = n, left = l, right = r}) = toListRB l ++ replicate n x ++ toListRB r

fromListRB :: (Ord a) => [a] -> RBNode a
fromListRB = foldr insertRB Nil

filterRB :: (Ord a) => (a -> Bool) -> RBNode a -> RBNode a
filterRB p root = foldr step Nil (toListRB root)
  where
    step x acc = if p x then insertRB x acc else acc

----------------------------------------------------------------------------------------------------------

isRed :: RBNode a -> Bool
isRed Node {color = Red} = True
isRed _ = False

isBlack :: RBNode a -> Bool
isBlack Nil = True
isBlack Node {color = Black} = True
isBlack _ = False

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

rootBlack :: RBNode a -> Bool
rootBlack (Node {color = Red}) = False
rootBlack _ = True

validRBTree :: RBMultiSet a -> Bool
validRBTree multiset = rootBlack tree && noRedChildrenForRed tree && isJust (blackPathCount tree)
  where
    tree = getRBNode multiset
