{-# language GeneralizedNewtypeDeriving #-}
module Main where

import Control.Applicative
import Data.Maybe
import Data.Word
import Hedgehog.Internal.Tree
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck

main = verboseBatch (monad (undefined :: MaybeTree (Bool, Bool, Bool)))

newtype MaybeTree a = MaybeTree { unMaybeTree :: Tree Maybe a }
  deriving (Show, Functor, Applicative, Monad)

instance Arbitrary a => Arbitrary (MaybeTree a)
  where arbitrary = MaybeTree . Tree . fmap unMaybeNode <$> arbitrary

instance Eq a => Eq (MaybeTree a)
  where MaybeTree (Tree m0) == MaybeTree (Tree m1) = (MaybeNode <$> m0) == (MaybeNode <$> m1)

instance (Eq a, EqProp a) => EqProp (MaybeTree a)
  where (=-=) = eq

newtype MaybeNode a = MaybeNode { unMaybeNode :: Node Maybe a }
  deriving (Show, Functor, Applicative, Monad)

instance Arbitrary a => Arbitrary (MaybeNode a)
  where arbitrary = do
          a <- arbitrary
          trees <- fmap unMaybeTree . maybeToList <$> arbitrary -- TODO: The lists may be a bit larger
          return $ MaybeNode (Node a trees)

instance Eq a => Eq (MaybeNode a)
  where MaybeNode (Node a0 ts0) == MaybeNode (Node a1 ts1) = a0 == a1 && (MaybeTree <$> ts0) == (MaybeTree <$> ts1)
