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

### Основные типы

```haskell
data Color = Red | Black deriving (Show, Eq)

data RBNode a
  = Nil
  | Node Color a Int (RBNode a) (RBNode a)
  deriving (Show)
```

 `Color` — цвет узла.<br>
 `RBNode a` — узел дерева: значение, количество повторений, левое и правое поддеревья.<br>
 `Nil` - пустое дерево.

 ### Реализованные экземпляры классов типов

`Eq (RBNode a)`
``` haskell
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
```
Собственная реализация сравнения деревьев без преобразования их в списки.
Используется итеративный обход: функция pushLefts проходит по левым узлам, а nextElem достаёт следующий элемент.

<br>

`Semigroup (RBNode a)`
```haskell
instance (Ord a) => Semigroup (RBNode a) where
  a <> b = fromList (toList a ++ toList b)
```
Операция объединения (<>) определена через конкатенацию списков элементов обоих деревьев с последующим построением нового сбалансированного дерева.

<br>

`Monoid (RBNode a)`
```haskell
instance (Ord a) => Monoid (RBNode a) where
  mempty = Nil
```

mempty задаёт нейтральный элемент для операции объединения — пустое дерево (Nil).<br>
Совместно с Semigroup это формирует корректную моноидную структуру для мультимножества.


### Основные функции

`insertRB x`
```haskell
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
```
Добавление элемента в дерево с последующей балансировкой.
Если элемент уже присутствует, увеличивается его счётчик.
Балансировка выполняется по стандартным правилам красно-чёрных деревьев:
перекрашивание и повороты, обеспечивающие сохранение инвариантов.

<br>

`deleteRB x`
```haskell
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
```
Удаление элемента из дерева с сохранением его свойств.
Если у элемента несколько копий, счётчик просто уменьшается.
Иначе — выполняется корректирующая перестройка узлов, восстанавливающая балансировку.
Включает обработку различных случаев, включая двойной черный узел.

<br>

`toList RBTree`
```haskell
toList :: RBNode a -> [a]
toList Nil = []
toList (Node _ x count left right) = toList left ++ replicate count x ++ toList right
```
Рекурсивное преобразование дерева в отсортированный список с учётом кратности каждого элемента.

<br>

`fromList list`
```haskell
fromList :: (Ord a) => [a] -> RBNode a
fromList = foldr insertRB Nil
```
Создаёт сбалансированное дерево из списка элементов при помощи правой свёртки.
Каждый элемент вставляется по одному с балансировкой, что обеспечивает корректную структуру.

<br>

`filterRB predicate RBTree`
```haskell
filterRB :: (Ord a) => (a -> Bool) -> RBNode a -> RBNode a
filterRB _ Nil = Nil
filterRB p (Node color x count left right) = if p x then Node color x count newLeft newRight else newLeft <> newRight
  where
    newLeft = filterRB p left
    newRight = filterRB p right
```
Фильтрация элементов по предикату.
Если элемент удовлетворяет условию, он сохраняется, иначе - исключается.
Рекурсивно создаются новые поддеревья и объединяются с помощью <>.


<br>

`mapRB f RBTree`
```haskell
mapRB :: (Ord a, Ord b) => (a -> b) -> RBNode a -> RBNode b
mapRB _ Nil = Nil
mapRB p (Node color x count left right) = Node color (p x) count newLeft newRight
  where
    newLeft = mapRB p left
    newRight = mapRB p right
```
Преобразует каждый элемент дерева функцией f.
Результат строится рекурсивно в виде нового красно-чёрного дерева.

<br>

`foldlRB f init RBTree`
```haskell
foldlRB :: (a -> b -> a) -> a -> RBNode b -> a
foldlRB _ acc Nil = acc
foldlRB f acc (Node _ x count left right) = foldlRB f accNode right
  where
    accNode = iterate (`f` x) accLeft !! count
    accLeft = foldlRB f acc left
```
Левая свёртка дерева — обходит его в порядке возрастания.
Для каждого элемента применяет функцию f, обновляя аккумулятор.
Учитывает кратность элементов (count) через повторное применение функции.

<br>

`foldrRB f init RBTree`
```haskell
foldrRB :: (a -> b -> b) -> b -> RBNode a -> b
foldrRB _ acc Nil = acc
foldrRB f acc (Node _ x count left right) = foldrRB f accNode left
  where
    accNode = iterate (f x) accRight !! count
    accRight = foldrRB f acc right
```
Левая свёртка дерева — обходит его в порядке убывания.
Для каждого элемента применяет функцию f, обновляя аккумулятор.
Учитывает кратность элементов (count) через повторное применение функции.


## Тестирование

### Unit-тесты

Проверяют корректность всех основных функций:
```haskell
testCase "mempty tree toList is []" $
  toList (mempty :: RBNode Int) @?= [],
testCase "insertRB and fromList" $
  toList (fromList [3, 1, 2, 1 :: Int]) @?= [1, 1, 2, 3],
testCase "deleteRB removes one occurrence" $
  toList (deleteRB 2 (fromList [3, 1, 2, 1, 1 :: Int])) @?= [1, 1, 1, 3],
testCase "deleteRB removes multiple occurrences" $
  toList (deleteRB 1 (fromList [3, 1, 2, 1, 1 :: Int])) @?= [1, 1, 2, 3],
testCase "filterRB keeps even elements" $
  toList (filterRB even (fromList [1, 2, 3, 4, 5 :: Int])) @?= [2, 4],
testCase "mapRB doubles elements" $
  toList (mapRB (* 2) (fromList [1, 2, 3 :: Int])) @?= [2, 4, 6],
testCase "foldlRB divides elements" $
  foldlRB div 16 (fromList [1, 2, 4 :: Int]) @?= 2,
testCase "foldrRB sums elements" $
  foldrRB (+) 0 (fromList [1, 2, 3, 4 :: Int]) @?= 10,
testCase "Eq: same multiset (different order)" $
  fromList [10, 5, 15, 3, 7, 3 :: Int] @=? fromList [5, 3, 7, 10, 15, 3],
testCase "Eq: different multisets" $
  False @=? (fromList [1, 2 :: Int] == fromList [1, 1])
``` 

### Property-based тесты

Проверяют свойства моноида (нейтральный элемент, ассоциативность) и корректность сравнения (Eq) относительно семантики мультимножества:

```haskell
QC.testProperty "Monoid: left identity" $
  \(xs :: [Int]) -> mempty <> fromList xs == (fromList xs :: RBNode Int),
QC.testProperty "Monoid: right identity" $
  \(xs :: [Int]) -> fromList xs <> mempty == (fromList xs :: RBNode Int),
QC.testProperty "Monoid: associativity" $
  \(xs :: [Int]) (ys :: [Int]) (zs :: [Int]) ->
    let a = fromList xs; b = fromList ys; c = fromList zs
      in (a <> b) <> c == (a <> (b <> c) :: RBNode Int),
QC.testProperty "Eq consistent with sorted list equality" $
  \(xs :: [Int]) (ys :: [Int]) ->
    (fromList xs == fromList ys) === (sort xs == sort ys)
```

## Выводы

В ходе работы была реализована собственная структура данных — красно-чёрное дерево для представления мультисета. Это позволило на практике применить полиморфизм, рекурсию и инкапсуляцию, а также освоить реализацию и использование экземпляров классов типа для задания обобщённого поведения.<br>
Были применены различные приёмы функционального программирования: рекурсивный обход дерева, свёртки, отображения и фильтрация с сохранением балансировки.<br>
Для проверки корректности реализации использованы как unit-тесты, так и property-based, что позволило убедиться в устойчивости и универсальности реализованных функций.



