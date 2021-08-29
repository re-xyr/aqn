-- This module is originally written by Olle Fredrickson (https://github.com/ollef).
-- Original source is https://github.com/ollef/sixty/blob/main/src/Data/Tsil.hs under BSD-3 License.
module Data.Tsil where

import           Control.Applicative (Alternative (empty, (<|>)))
import           Control.Monad       (ap)
import           Control.Monad.Zip   (MonadZip (mzipWith))
import           Data.Bifunctor      (Bifunctor (first, second))
import           Data.Foldable       (Foldable (toList), sequenceA_)
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as Seq
import           GHC.Exts            (IsList (Item, fromList, toList))

-- | A list that appends instead of prepends elements.
-- It is strict in its length, but lazy in elements.
data List a
  = Empty
  | List a :> a
  deriving (Eq, Functor, Ord, Traversable)

instance Show a => Show (List a) where
  show = show . Data.Foldable.toList

instance Semigroup (List a) where
  xs <> Empty     = xs
  xs <> (ys :> y) = (xs <> ys) :> y

instance Monoid (List a) where
  mempty = Empty
  mappend = (<>)

instance Applicative List where
  pure = (Empty :>)
  (<*>) = ap

instance Alternative List where
  empty = Empty
  (<|>) = mappend

instance Monad List where
  return = pure
  Empty >>= _   = Empty
  xs :> x >>= f = (xs >>= f) <> f x

instance MonadZip List where
  mzipWith = Data.Tsil.zipWith

instance Foldable List where
  foldMap _ Empty     = mempty
  foldMap f (xs :> x) = foldMap f xs `mappend` f x
  toList = go []
    where
      go acc Empty     = acc
      go acc (xs :> x) = go (x : acc) xs

instance IsList (List a) where
  type Item (List a) = a
  fromList = go Empty
    where
      go acc []       = acc
      go acc (a : as) = go (acc :> a) as
  toList = Data.Foldable.toList

toList :: Foldable f => f a -> List a
toList = fromList . Data.Foldable.toList

toSeq :: Foldable f => f a -> Seq a
toSeq = Seq.fromList . Data.Foldable.toList

null :: List a -> Bool
null Empty    = True
null (_ :> _) = False

lookup :: Eq a => a -> List (a, b) -> Maybe b
lookup _ Empty = Nothing
lookup a (as :> (a', b))
  | a == a' = Just b
  | otherwise = Data.Tsil.lookup a as

filter :: (a -> Bool) -> List a -> List a
filter _ Empty = Empty
filter f (xs :> x)
  | f x = Data.Tsil.filter f xs :> x
  | otherwise = Data.Tsil.filter f xs

partition :: (a -> Bool) -> List a -> (List a, List a)
partition _ Empty = mempty
partition p (xs :> x)
  | p x = first (:> x) $ partition p xs
  | otherwise = second (:> x) $ partition p xs

span :: (a -> Bool) -> List a -> (List a, List a)
span _ Empty = (Empty, Empty)
span p as@(as' :> a)
  | p a = second (:> a) $ Data.Tsil.span p as'
  | otherwise = (as, Empty)

zip :: List a -> List b -> List (a, b)
zip = Data.Tsil.zipWith (,)

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith _ Empty _             = Empty
zipWith _ _ Empty             = Empty
zipWith f (as :> a) (bs :> b) = Data.Tsil.zipWith f as bs :> f a b

zipWithM :: Monad m => (a -> b -> m c) -> List a -> List b -> m (List c)
zipWithM f as bs = sequenceA (Data.Tsil.zipWith f as bs)

zipWithM_ :: Monad m => (a -> b -> m c) -> List a -> List b -> m ()
zipWithM_ f as bs = sequenceA_ (Data.Tsil.zipWith f as bs)

unzip :: List (a, b) -> (List a, List b)
unzip Empty = (Empty, Empty)
unzip (as :> (a, b)) = (as' :> a, bs' :> b)
  where
    (as', bs') = Data.Tsil.unzip as
