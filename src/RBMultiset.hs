module RBMultiset (RBNode, insertRB, deleteRB, filterRB, mapRB, foldlRB, foldrRB, toList, fromList) where

data Color = Red | Black deriving (Show, Eq)

data RBNode a
  = Nil
  | Node Color a Int (RBNode a) (RBNode a)
  deriving (Show)

instance (Ord a) => Eq (RBNode a) where
  (==) tree1 tree2 = areEqual (pushLefts tree1 []) (pushLefts tree2 [])
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
      pushLefts node@(Node _ _ _ left _) nodes = pushLefts left (node : nodes)

      nextElem :: [RBNode a] -> Maybe ((a, Int), [RBNode a])
      nextElem [] = Nothing
      nextElem (Node _ x count _ right : nodes) = Just ((x, count), pushLefts right nodes)
      nextElem (Nil : _) = error "unexpected Nil in nodes"

instance (Ord a) => Semigroup (RBNode a) where
  a <> b = fromList (toList a ++ toList b)

instance (Ord a) => Monoid (RBNode a) where
  mempty = Nil

insertRB :: (Ord a) => a -> RBNode a -> RBNode a
insertRB x tree = makeBlack (ins tree)
  where
    ins Nil = Node Red x 1 Nil Nil
    ins (Node color y count left right)
      | x < y = balance (Node color y count (ins left) right)
      | x > y = balance (Node color y count left (ins right))
      | otherwise = Node color y (count + 1) left right
    makeBlack (Node _ y count left right) = Node Black y count left right
    makeBlack Nil = Nil

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
deleteRB x tree = makeBlack (del tree)
  where
    makeBlack Nil = Nil
    makeBlack (Node _ y count left right) = Node Black y count left right

    del Nil = Nil
    del node@(Node color y count left right)
      | x < y = fixL (Node color y count (del left) right)
      | x > y = fixR (Node color y count left (del right))
      | count > 1 = Node color x (count - 1) left right
      | otherwise = removeNode node

    fixL :: RBNode a -> RBNode a
    fixL (Node color y count Nil right) = fixDoubleBlackL (Node color y count Nil right)
    fixL (Node color y count left right) = Node color y count left right
    fixL Nil = Nil

    fixR :: RBNode a -> RBNode a
    fixR (Node color y count left Nil) = fixDoubleBlackR (Node color y count left Nil)
    fixR (Node color y count left right) = Node color y count left right
    fixR Nil = Nil

    removeNode (Node Red _ _ left right) =
      case (left, right) of
        (Nil, Nil) -> Nil
        _ -> fixR $ Node Red newValue newCount left newRight where (newValue, newCount, newRight) = deleteMin right
    removeNode (Node Black _ _ left right) =
      case (left, right) of
        (Nil, Nil) -> Nil
        (Nil, node) -> recolor node Black
        (node, Nil) -> recolor node Black
        _ -> Node Black newValue newCount left newRight where (newValue, newCount, newRight) = deleteMin right
    removeNode Nil = Nil

    recolor :: RBNode a -> Color -> RBNode a
    recolor Nil _ = Nil
    recolor (Node _ x count left right) color = Node color x count left right

    deleteMin :: RBNode a -> (a, Int, RBNode a)
    deleteMin (Node color x count Nil right) = (x, count, recolor right color)
    deleteMin (Node color x count left right) =
      let (minVal, minCount, newLeft) = deleteMin left
       in (minVal, minCount, Node color x count newLeft right)
    deleteMin Nil = error "deleteMin on empty tree"

    fixDoubleBlackL :: RBNode a -> RBNode a
    fixDoubleBlackL (Node c gv gc (Node Red pv pc pl pr) gr) =
      balanceDelL (Node Black pv pc (Node Red gv gc Nil pl) pr) gr
    fixDoubleBlackL (Node c gv gc (Node Black pv pc pl pr) gr)
      | isBlack pl && isBlack pr =
          Node Black gv gc (Node Red pv pc pl pr) gr
    fixDoubleBlackL (Node c gv gc (Node Black pv pc (Node Red cv cc cl cr) pr) gr) =
      Node
        Black
        cv
        cc
        (Node Black gv gc Nil cl)
        (Node Black pv pc cr pr)
    fixDoubleBlackL (Node c gv gc (Node Black pv pc pl (Node Red cv cc cl cr)) gr) =
      Node
        Black
        pv
        pc
        (Node Black gv gc Nil pl)
        (Node Red cv cc cl cr)
    fixDoubleBlackL t = recolor t Black

    fixDoubleBlackR :: RBNode a -> RBNode a
    fixDoubleBlackR (Node c gv gc gl (Node Red pv pc pl pr)) =
      balanceDelR (Node Black pv pc pl (Node Red gv gc pr Nil)) gl
    fixDoubleBlackR (Node c gv gc gl (Node Black pv pc pl pr))
      | isBlack pl && isBlack pr =
          Node Black gv gc gl (Node Red pv pc pl pr)
    fixDoubleBlackR (Node c gv gc gl (Node Black pv pc pl (Node Red cv cc cl cr))) =
      Node
        Black
        cv
        cc
        (Node Black pv pc pl cl)
        (Node Black gv gc cr Nil)
    fixDoubleBlackR (Node c gv gc gl (Node Black pv pc (Node Red cv cc cl cr) pr)) =
      Node
        Black
        pv
        pc
        (Node Red cv cc cl cr)
        (Node Black gv gc pr Nil)
    fixDoubleBlackR t = recolor t Black

    balanceDelL :: RBNode a -> RBNode a -> RBNode a
    balanceDelL (Node Black gv gc (Node Red pv pc (Node Red cv cc cl cr) s) gr) _ =
      Node Red pv pc (Node Black cv cc cl cr) (Node Black gv gc s gr)
    balanceDelL (Node Black gv gc (Node Red pv pc cl (Node Red cv cc cr s)) gr) _ =
      Node Red cv cc (Node Black pv pc cl cr) (Node Black gv gc s gr)
    balanceDelL node _ = node

    balanceDelR :: RBNode a -> RBNode a -> RBNode a
    balanceDelR (Node Black gv gc gl (Node Red pv pc s (Node Red cv cc cr gr))) _ =
      Node Red pv pc (Node Black gv gc gl s) (Node Black cv cc cr gr)
    balanceDelR (Node Black gv gc gl (Node Red pv pc (Node Red cv cc s cr) gr)) _ =
      Node Red cv cc (Node Black gv gc gl s) (Node Black pv pc cr gr)
    balanceDelR node _ = node

    isBlack :: RBNode a -> Bool
    isBlack Nil = True
    isBlack (Node Black _ _ _ _) = True
    isBlack _ = False

toList :: RBNode a -> [a]
toList Nil = []
toList (Node _ x count left right) = toList left ++ replicate count x ++ toList right

fromList :: (Ord a) => [a] -> RBNode a
fromList = foldr insertRB Nil

filterRB :: (Ord a) => (a -> Bool) -> RBNode a -> RBNode a
filterRB _ Nil = Nil
filterRB p (Node color x count left right) = if p x then Node color x count newLeft newRight else newLeft <> newRight
  where
    newLeft = filterRB p left
    newRight = filterRB p right

mapRB :: (Ord a, Ord b) => (a -> b) -> RBNode a -> RBNode b
mapRB _ Nil = Nil
mapRB p (Node color x count left right) = Node color (p x) count newLeft newRight
  where
    newLeft = mapRB p left
    newRight = mapRB p right

foldlRB :: (a -> b -> a) -> a -> RBNode b -> a
foldlRB _ acc Nil = acc
foldlRB f acc (Node _ x count left right) = foldlRB f accNode right
  where
    accNode = iterate (`f` x) accLeft !! count
    accLeft = foldlRB f acc left

foldrRB :: (a -> b -> b) -> b -> RBNode a -> b
foldrRB _ acc Nil = acc
foldrRB f acc (Node _ x count left right) = foldrRB f accNode left
  where
    accNode = iterate (f x) accRight !! count
    accRight = foldrRB f acc right
