{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Grouped where

import Control.Arrow
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Foldable as Fol
import qualified Data.Map as Map

import Data.HList.HList (HList)
import qualified Data.HList.HList as HList



data Grouped (ks :: [*]) (a :: *) where
  Value :: a -> Grouped '[] a
  Grouped :: Map k (Grouped ks a) -> Grouped (k ': ks) a

instance Functor (Grouped '[]) where
  fmap f (Value x) = Value $ f x

instance Functor (Grouped ks) => Functor (Grouped (k ': ks)) where
  fmap f (Grouped m) = Grouped $ (fmap . fmap) f m

instance Foldable (Grouped '[]) where
  foldr f b (Value a) = f a b

instance Foldable (Grouped ks) => Foldable (Grouped (k ': ks)) where
  foldr f b (Grouped m) = Fol.foldr (\g b' -> Fol.foldr f b' g) b m

instance Traversable (Grouped '[]) where
  traverse f (Value x) = Value <$> f x

instance Traversable (Grouped ks) => Traversable (Grouped (k ': ks)) where
  traverse f (Grouped m) = Grouped <$> (traverse . traverse) f m



type family Append (a :: *) (as :: [*]) :: [*] where
  Append x '[] = '[x]
  Append x (y ': ys) = y ': Append x ys

type family Last (as :: [*]) :: * where
  Last (x ': '[]) = x
  Last (x ': y ': ys) = Last (y ': ys)

type family NestedMap (ks :: [*]) (a :: *) :: * where
  NestedMap '[] a = a
  NestedMap (k ': ks) a = Map k (NestedMap ks a)

type family NestedAssocs (ks :: [*]) (a :: *) :: * where
  NestedAssocs '[] a = a
  NestedAssocs (k ': ks) a = [(k, NestedAssocs ks a)]



-- Producing grouped data

fromValue :: a -> Grouped '[] a
fromValue = Value

groupBy :: Ord k => (a -> k) -> Grouped ks [a] -> Grouped (Append k ks) [a]
groupBy f (Value xs) = Grouped $ fmap Value m
  where
    m = Map.fromListWith (++) . map (\x -> (f x, [x])) $ xs
groupBy f (Grouped m) = Grouped $ fmap (groupBy f) m



-- Consuming grouped data

toValue :: Grouped '[] a -> a
toValue (Value x) = x

groups :: Grouped (k ': ks) a -> Map k (Grouped ks a)
groups (Grouped m) = m

groupKeys :: Grouped (k ': ks) a -> [k]
groupKeys (Grouped m) = Map.keys m

subgroups :: Grouped (k ': ks) a -> [Grouped ks a]
subgroups (Grouped m) = Map.elems m

toMap :: Grouped ks a -> NestedMap ks a
toMap (Value x) = x
toMap (Grouped m) = fmap toMap m

nestedAssocs :: Grouped ks a -> NestedAssocs ks a
nestedAssocs (Value x) = x
nestedAssocs (Grouped m) = Map.assocs $ fmap nestedAssocs m

hassocs :: Grouped ks a -> [(HList ks, a)]
hassocs (Value x) = [(HList.HNil, x)]
hassocs (Grouped m) = concatMap hconsFst . Map.assocs . fmap hassocs $ m
  where
    hconsFst (k, xs) = map (HList.HCons k *** id) xs

assocs :: HList.HTuple ks t => Grouped ks a -> [(t, a)]
assocs = map (HList.hToTuple *** id) . hassocs

keys :: HList.HTuple ks t => Grouped ks a -> [t]
keys = map fst . assocs

mapGroupsWithKey
  :: (k -> Grouped ks a -> Grouped ks b)
  -> Grouped (k ': ks) a
  -> Grouped (k ': ks) b
mapGroupsWithKey f (Grouped m) = Grouped $ Map.mapWithKey f m

mapWithHkey :: (HList ks -> a -> b) -> Grouped ks a -> Grouped ks b
mapWithHkey f (Value x) = Value $ f HList.HNil x
mapWithHkey f (Grouped m) = Grouped $ Map.mapWithKey g m
  where
    g k = mapWithHkey (f . HList.HCons k)

mapWithKey :: HList.HTuple ks t => (t -> a -> b) -> Grouped ks a -> Grouped ks b
mapWithKey f = mapWithHkey (f . HList.hToTuple)

foldrGroupsWithKey
  :: (k -> Grouped ks a -> b -> b) -> b -> Grouped (k ': ks) a -> b
foldrGroupsWithKey f b (Grouped m) = Map.foldrWithKey f b m

foldrWithHkey :: (HList ks -> a -> b -> b) -> b -> Grouped ks a -> b
foldrWithHkey f b (Value x) = f HList.HNil x b
foldrWithHkey f b (Grouped m) = Map.foldrWithKey g b m
  where
    g k grpd b' = foldrWithHkey (f . HList.HCons k) b' grpd

foldrWithKey
  :: HList.HTuple ks t => (t -> a -> b -> b) -> b -> Grouped ks a -> b
foldrWithKey f = foldrWithHkey (f . HList.hToTuple)

traverseGroupsWithKey_
  :: Applicative f => (k -> Grouped ks a -> f b) -> Grouped (k ': ks) a -> f ()
traverseGroupsWithKey_ f = foldrGroupsWithKey g (pure ())
  where
    g k grpd b = f k grpd *> b

traverseWithHkey_
  :: Applicative f => (HList ks -> a -> f b) -> Grouped ks a -> f ()
traverseWithHkey_ f = foldrWithHkey g (pure ())
  where
    g ks a b = f ks a *> b

traverseWithKey_ ::
  (Applicative f, HList.HTuple ks t) => (t -> a -> f b) -> Grouped ks a -> f ()
traverseWithKey_ f = traverseWithHkey_ (f . HList.hToTuple)

forGroupsWithKey_
  :: Applicative f => Grouped (k ': ks) a -> (k -> Grouped ks a -> f b) -> f ()
forGroupsWithKey_ = flip traverseGroupsWithKey_

forWithHkey_
  :: Applicative f => Grouped ks a -> (HList ks -> a -> f b) -> f ()
forWithHkey_ = flip traverseWithHkey_

forWithKey_ ::
  (Applicative f, HList.HTuple ks t) => Grouped ks a -> (t -> a -> f b) -> f ()
forWithKey_ = flip traverseWithKey_



-- Transforming grouped data

class OuterJoin (ks :: [*]) where
  outerJoin :: (k ~ Last ks, Ord k) => Set k -> Grouped ks a -> Grouped ks (Maybe a)

instance OuterJoin (a ': '[]) where
  outerJoin s (Grouped m) =
    let m' = (fmap . fmap) Just m
        s' = Map.fromSet (const $ Value Nothing) s
    in  Grouped $ m' `Map.union` s'

instance OuterJoin (b ': bs) => OuterJoin (a ': b ': bs) where
  outerJoin s (Grouped m) = Grouped $ fmap (outerJoin s) m
