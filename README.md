# Лабораторная работа №2

### **Павличенко Софья P3315**

**Вариант**: rb-bag

## Задание

В рамках лабораторной работы вам предлагается реализовать одну из предложенных классических структур данных (список, дерево, бинарное дерево, hashmap, граф...).

Требования:
1. Функции:
    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая);
    - структура должна быть моноидом.
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.
7. Обратите внимание:
    - API должно быть реализовано для заданного интерфейса и оно не должно "протекать". На уровне тестов -- в первую очередь нужно протестировать именно API (dict, set, bag).
    - Должна быть эффективная реализация функции сравнения (не наивное приведение к спискам, их сортировка с последующим сравнением), реализованная на уровне API, а не внутреннего представления.

## Реализация

### Интерфейс MultiSet

Для унификации работы с мультимножеством был определен класс типов:

```haskell
class (Foldable bag) => MultiSet bag where
  empty :: (Ord a) => bag a
  insert :: (Ord a) => a -> bag a -> bag a
  delete :: (Ord a) => a -> bag a -> bag a
  map :: (Ord b) => (a -> b) -> bag a -> bag b
  filter :: (Ord a) => (a -> Bool) -> bag a -> bag a
  toList :: bag a -> [a]
  fromList :: (Ord a) => [a] -> bag a
```

### Типы данных

Основная структура данных реализована с использованием **Record Syntax**.

```haskell
data Color = Red | Black deriving (Show, Eq)

data RBNode a
  = Nil
  | Node
      { color :: Color,
        value :: a,
        count :: Int,      -- Количество вхождений элемента
        left :: RBNode a,
        right :: RBNode a
      }
  deriving (Show)

newtype RBMultiSet a = RBMultiSet (RBNode a) deriving (Show)
```

`RBMultiSet` является оберткой (`newtype`) над `RBNode`, скрывая детали реализации дерева от пользователя.

### Экземпляры классов типов

**`MultiSet`**<br>
Реализация основного интерфейса делегирует вызовы внутренним функциям модуля `RBMultiSet`.

```haskell
instance MultiSet RBMultiSet where
  empty = mempty
  insert x (RBMultiSet tree) = RBMultiSet (insertRB x tree)
  delete x (RBMultiSet tree) = RBMultiSet (deleteRB x tree)
  map f (RBMultiSet tree) = fromList (toListRB (fmap f tree))
  filter p (RBMultiSet tree) = RBMultiSet (filterRB p tree)
  toList (RBMultiSet tree) = toListRB tree
  fromList xs = RBMultiSet (fromListRB xs)
```

**`Eq`**<br>
Собственная реализация сравнения деревьев без преобразования их в списки.
Используется итеративный обход: функция pushLefts проходит по левым узлам, а nextElem достаёт следующий элемент.

```haskell
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
```

**`Semigroup` и `Monoid`**<br>
Реализуют объединение двух мультимножеств и нейтральный элемент (пустое дерево).

```haskell
instance (Ord a) => Semigroup (RBMultiSet a) where
  (RBMultiSet a) <> (RBMultiSet b) = RBMultiSet (unionRB a b)

instance (Ord a) => Monoid (RBMultiSet a) where
  mempty = RBMultiSet Nil
```

**`Foldable`**<br>
Так как класс типов `MultiSet` требует наличие экземпляра `Foldable`, он был реализован для типа-обертки `RBMultiSet`.
Реализация делегирует работу внутреннему типу `RBNode`, который определяет непосредственную логику обхода дерева с учетом кратности элементов.

```haskell
instance Foldable RBMultiSet where
  foldMap f (RBMultiSet node) = foldMap f node


instance Foldable RBNode where
  foldMap _ Nil = mempty
  foldMap f (Node {value = x, count = n, left = l, right = r}) = 
    foldMap f l <> mconcat (replicate n (f x)) <> foldMap f r
```

**`Functor`**<br>
Реализован для `RBNode` для поддержки `fmap` внутри реализации `map` для `MultiSet`.

```haskell
instance Functor RBNode where
  fmap _ Nil = Nil
  fmap f node@(Node {value = x, left = l, right = r}) = 
    node {value = f x, left = fmap f l, right = fmap f r}
```

### Основные алгоритмы

**Вставка (`insertRB`)**<br>
Использует стандартный алгоритм вставки в красно-черное дерево с последующей балансировкой. Если элемент уже существует, увеличивается счетчик `count`.

```haskell
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
```

**Удаление (`deleteRB`)**<br>
Сначала проверяется счетчик элемента: если он > 1, то просто уменьшаем `count`. Если равен 1, запускается алгоритм удаления узла с перебалансировкой дерева для сохранения инвариантов красно-черного дерева. 

```haskell
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
```

**Объединение (`unionRB`)**<br>
Объединение двух деревьев реализовано через свертку одного дерева с последовательной вставкой элементов в другое.

```haskell
unionRB :: (Ord a) => RBNode a -> RBNode a -> RBNode a
unionRB Nil t = t
unionRB t Nil = t
unionRB t1 t2 = foldr insertRB t2 (toListRB t1)
```

**Фильтрация (`filterRB`)**<br>
Реализована через построение нового дерева, используя свёртку.

```haskell
filterRB :: (Ord a) => (a -> Bool) -> RBNode a -> RBNode a
filterRB p root = foldr step Nil (toListRB root)
  where
    step x acc = if p x then insertRB x acc else acc
```

**Проверка валидности дерева**<br>
Для тестов были реализованы функции проверки инвариантов КЧ-дерева:
1. Корень черный.
2. У красного узла нет красных детей.
3. "Черная высота" одинакова на всех путях от корня к листьям.

```haskell
validRBTree :: RBMultiSet a -> Bool
validRBTree multiset = rootBlack tree && noRedChildrenForRed tree && isJust (blackPathCount tree)
  where tree = getRBNode multiset
```

## Тестирование

### Unit-тесты
Проверяют базовую корректность API `MultiSet` и `RBMultiSet`.

```haskell
testCase "mempty tree toList is []" $
  toList (mempty :: RBMultiSet Int) @?= [],

testCase "union (<>) combines elements" $
  toList ((fromList [1, 2, 3] :: RBMultiSet Int) <> (fromList [4, 5, 3] :: RBMultiSet Int)) @?= [1, 2, 3, 3, 4, 5],

testCase "insertRB and fromList" $
  toList (fromList [3, 1, 2, 1] :: RBMultiSet Int) @?= [1, 1, 2, 3],

testCase "deleteRB removes one occurrence" $
  toList (delete 2 (fromList [3, 1, 2, 1, 1] :: RBMultiSet Int)) @?= [1, 1, 1, 3],

testCase "deleteRB removes multiple occurrences" $
  toList (delete 1 (fromList [3, 1, 2, 1, 1] :: RBMultiSet Int)) @?= [1, 1, 2, 3],

testCase "deleteRB removes zero occurrences" $
  toList (delete 4 (fromList [3, 1, 2, 1, 1] :: RBMultiSet Int)) @?= [1, 1, 1, 2, 3],

testCase "filterRB keeps even elements" $
  toList (filter even (fromList [1, 2, 3, 4, 5] :: RBMultiSet Int)) @?= [2, 4],

testCase "mapRB doubles elements" $
  toList (map (* 2) (fromList [1, 2, 3] :: RBMultiSet Int)) @?= [2, 4, 6],

testCase "foldlRB divides elements" $
  foldl div 16 (fromList [1, 2, 4] :: RBMultiSet Int) @?= 2,

testCase "foldrRB sums elements" $
  foldr (+) 1 (fromList [1, 2, 3, 4] :: RBMultiSet Int) @?= 11,

testCase "Eq: same multiset (different order)" $
  (fromList [10, 5, 15, 3, 7, 3] :: RBMultiSet Int) @=? fromList [5, 3, 7, 10, 15, 3],

testCase "Eq: different multisets" $
  False @=? ((fromList [1, 2] :: RBMultiSet Int) == fromList [1, 1])
```

### Property-based тесты
Проверяют свойства моноида (нейтральный элемент, ассоциативность), корректность сравнения (Eq) относительно семантики мультимножества и сохранение инвариантов дерева при модификациях.

```haskell
QC.testProperty "Monoid: left identity" $
  \(xs :: [Int]) -> mempty <> fromList xs == (fromList xs :: RBMultiSet Int),

QC.testProperty "Monoid: right identity" $
  \(xs :: [Int]) -> fromList xs <> mempty == (fromList xs :: RBMultiSet Int),

QC.testProperty "Monoid: associativity" $
  \(xs :: [Int]) (ys :: [Int]) (zs :: [Int]) ->
    let a = fromList xs; b = fromList ys; c = fromList zs
      in (a <> b) <> c == (a <> (b <> c) :: RBMultiSet Int),

QC.testProperty "Eq consistent with sorted list equality" $
  \(xs :: [Int]) (ys :: [Int]) ->
    ((fromList xs :: RBMultiSet Int) == fromList ys) === (sort xs == sort ys),

QC.testProperty "RBTree: valid after fromList (insert)" $
  \(xs :: [Int]) -> validRBTree (fromList xs :: RBMultiSet Int),

QC.testProperty "RBTree: valid after delete" $
  \(xs :: [Int]) (y :: Int) ->
    let ms0 = fromList xs :: RBMultiSet Int
        ms1 = insert y ms0
        ms2 = delete y ms1
      in validRBTree ms0 && validRBTree ms1 && validRBTree ms2,

QC.testProperty "RBTree: valid after filter" $
  \(xs :: [Int]) ->
    let ms0 = fromList xs :: RBMultiSet Int
        ms1 = filter even ms0
      in validRBTree ms0 && validRBTree ms1
```

## Выводы

В ходе работы была реализована собственная структура данных — красно-чёрное дерево для представления мультисета. Это позволило на практике применить полиморфизм, рекурсию и инкапсуляцию, а также освоить реализацию и использование экземпляров классов типа для задания обобщённого поведения.<br>
Были применены различные приёмы функционального программирования: рекурсивный обход дерева, свёртки, отображения и фильтрация с сохранением балансировки.<br>
Для проверки корректности реализации использованы как unit-тесты, так и property-based, что позволило убедиться в устойчивости и универсальности реализованных функций.