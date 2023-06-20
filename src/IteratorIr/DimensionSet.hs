{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module IteratorIr.DimensionSet (
    DimensionName,
    Dimension(Dim),
    Intersect,
    MaybeIntersect,
    Names,
    Delete,
    Union,
    GetDim,
    DimIndex,
    dimIndex,
    DimIndices,
    dimIndices,
    UDIR,
    Index(Index),
    index,
    indexAtDim,
    shiftIndexTo,
    shiftIndexBy
    --extendIndex
) where


import GHC.TypeLits (CmpNat, KnownNat, Nat, natVal, Symbol, type (+))
import Data.Proxy (Proxy(Proxy))

import Control.Lens.Traversal (element)
import Control.Lens.Setter ((.~))

-- compile-time dimension names
type DimensionName = Symbol

-- compile-time dimension with associated index range
data Dimension = Dim { name :: DimensionName, range :: (Nat, Nat) }


-- helpers for computations on index ranges:

type family Compare (a :: k) (b :: k) :: Ordering

type instance Compare (a :: Nat) b = CmpNat a b

-- in newest Haskell, Min and Max are already available in Data.Type.Ord
type family OrdCond o lt eq gt where
    OrdCond 'LT lt eq gt = lt
    OrdCond 'EQ lt eq gt = eq
    OrdCond 'GT lt eq gt = gt

type Min m n = OrdCond (Compare m n) m m n
type Max m n = OrdCond (Compare m n) n n m


-- computations on index ranges:

-- intersection of dimensions with matching name
type family Intersect (x :: Dimension) (y :: Dimension) :: Dimension where
    Intersect (Dim n '(a, b)) (Dim n '(c, d)) = Dim n '(Max a c, Min b d)

-- intersection of dimensions with matching name, with optional inputs
type family MaybeIntersect (x :: Maybe Dimension) (y :: Maybe Dimension) :: Dimension where
    MaybeIntersect (Just x) (Just y) = Intersect x y
    MaybeIntersect (Just x) Nothing = x
    MaybeIntersect Nothing (Just y) = y


-- general type-level list manipulation:

-- deletion of an element in a type-level list
type family Delete (x :: a) (xs :: [a]) :: [a] where
    Delete x (x ': xs) = Delete x xs
    Delete x (y ': xs) = y ': (Delete x xs)
    Delete x '[] = '[]

-- union of two type-level lists
type family Union (xs :: [a]) (ys :: [a]) :: [a] where
    Union (x ': xs) ys = x ': (Union xs (Delete x ys))
    Union '[] ys = ys
    Union xs '[] = xs


-- dimension list manipulation:

-- get dimension names from a list of dimensions
type family Names (ds :: [Dimension]) :: [Symbol] where
    Names ((Dim x r) ': ds) = x ': (Names ds)
    Names '[] = '[]

-- find a dimension by name in a list of dimensions
type family GetDim (x :: Symbol) (ds :: [Dimension]) :: Maybe Dimension where
    GetDim x ((Dim x r) ': ds) = Just (Dim x r)
    GetDim x ((Dim y r) ': ds) = GetDim x ds
    GetDim x '[] = Nothing

-- get the index of a dimension (by name) in a list of dimensions
type family DimIndex (x :: Symbol) (ds :: [Dimension]) :: Nat where
    DimIndex x ((Dim x r) ': xs) = 0
    DimIndex y (x ': xs) = 1 + (DimIndex y xs)

type family DimIndices (xs :: [Symbol]) (ds :: [Dimension]) :: [Nat] where
    DimIndices (x ': xs) ds = (DimIndex x ds) ': (DimIndices xs ds)
    DimIndices '[] _ = '[]

class IndexList (is :: [Nat]) where
    rtIndices :: Proxy is -> [Int]

instance IndexList '[] where
    rtIndices p = []

instance (KnownNat i, IndexList is) => IndexList (i ': is) where
    rtIndices p = (fromIntegral (natVal (Proxy @i))) : (rtIndices (Proxy @is))

-- get the index of a dimension in a list of dimensions as a run-time value
dimIndex :: forall i x ds. (i ~ DimIndex x ds, KnownNat i) => Proxy x -> Proxy ds -> Int
dimIndex a b = fromIntegral $ natVal $ Proxy @i

dimIndices :: forall is xs ds. (is ~ DimIndices xs ds, IndexList is) => Proxy xs -> Proxy ds -> [Int]
dimIndices a b = rtIndices $ Proxy @is

-- UDIR = UnionDimensionsIntersectRanges
type family UDIRImpl (ss :: [Symbol]) (ds :: [Dimension]) (es :: [Dimension]) :: [Dimension] where
    UDIRImpl (s ': ss) ds es = (MaybeIntersect (GetDim s ds) (GetDim s es)) ': (UDIRImpl ss ds es)
    UDIRImpl '[] ds es = '[]

type family UDIR (ds :: [Dimension]) (es :: [Dimension]) :: [Dimension] where
    UDIR ds es = UDIRImpl (Union (Names ds) (Names es)) ds es

-- very simple index type, using lists of Ints (thus not very safe)
data Index ds = Index { index :: [Int] } deriving (Eq, Show)

-- get the index value for a specific dimension by name
indexAtDim :: forall i x ds. (i ~ DimIndex x ds, KnownNat i) => Proxy x -> Index ds -> Int
indexAtDim dim idx = (index idx)!!(dimIndex dim (Proxy :: Proxy ds))

-- reset the index value for a specific dimension by name
shiftIndexTo :: forall i x ds. (i ~ DimIndex x ds, KnownNat i) => Proxy x -> Int -> Index ds -> Index ds
shiftIndexTo dim val idx = Index $ (element (dimIndex dim (Proxy :: Proxy ds)) .~ val) (index idx)

-- shift the index along a specific dimension by name
shiftIndexBy :: forall i x ds. (i ~ DimIndex x ds, KnownNat i) => Proxy x -> Int -> Index ds -> Index ds
shiftIndexBy dim offset idx = shiftIndexTo dim (indexAtDim dim idx + offset) idx

--extendIndex :: forall sds tds. (tds ~ [Dimension]) => Proxy tds -> Index sds -> Index tds
--extendIndex t i = Index @tds (dimIndices (Proxy @(Names tds)) (Proxy @sds))
