{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}

import Data.List (sort)
import RBMultiset
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

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
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property-based Tests"
    [ QC.testProperty "Monoid: left identity" $
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
    ]
