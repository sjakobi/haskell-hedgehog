{-# language GeneralizedNewtypeDeriving #-}
module Main where

import Data.Functor.Classes
import Hedgehog.Internal.Gen
import Hedgehog.Internal.Range
import Hedgehog.Internal.Seed
import Hedgehog.Internal.Tree
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck

main :: IO ()
main = do
  verboseBatch ("Node", concatMap unbatch
    [ applicative (undefined :: Node Maybe (Bool, Char, Int))
    , monad (undefined :: Node Maybe (Int, Bool, Char))
    , monadApplicative (undefined :: Node (Either Int) (Char, Bool))
    ])
  verboseBatch ("Node", concatMap unbatch
    [ applicative (undefined :: Tree Maybe (Bool, Char, Int))
    , monad (undefined :: Tree Maybe (Bool, Char, Int))
    , monadApplicative (undefined :: Tree (Either Int) (Char, Bool))
    ])

instance (Eq1 m, Eq a) => Eq (Tree m a) where
  Tree m0 == Tree m1 = liftEq (==) m0 m1

instance (Eq1 m, Eq a) => EqProp (Tree m a) where
  (=-=) = eq

instance (Arbitrary1 m, Arbitrary a) => Arbitrary (Tree m a)
  where arbitrary = Tree <$> arbitrary1

instance (Eq1 m, Eq a) => Eq (Node m a) where
  Node a0 ts0 == Node a1 ts1 = a0 == a1 && ts0 == ts1

instance (Eq1 m, Eq a) => EqProp (Node m a) where
  (=-=) = eq

instance (Arbitrary1 m, Arbitrary a) => Arbitrary (Node m a)
  where arbitrary = do
          a <- arbitrary
          n <- choose (0, 2)
          trees <- vector n
          return $ (Node a trees)

instance CoArbitrary Size
  where coarbitrary = coarbitraryIntegral

instance CoArbitrary Seed where
  coarbitrary (Seed

instance Arbitrary (GenT m a)
  where arbitrary = GenT <$> arbitrary
