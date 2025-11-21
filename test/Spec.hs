{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}

import Data.List (sort)
import MultiSet
import RBMultiSet
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Prelude hiding (filter, map)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "RBMultiset Tests"
    [ unitTests,
      propertyTests
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testCase "mempty tree toList is []" $
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
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property-based Tests"
    [ QC.testProperty "Monoid: left identity" $
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
      QC.testProperty "RBTree: valid after fromList" $
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
           in validRBTree ms0 && validRBTree ms1,
      QC.testProperty "RBTree: valid after map" $
        \(xs :: [Int]) ->
          let ms0 = fromList xs :: RBMultiSet Int
              ms1 = map (10 -) ms0
           in validRBTree ms0 && validRBTree ms1
    ]
