{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module DimensionSetTests (
    dimensionSetTests
) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import IteratorIr.DimensionSet

import Data.Proxy (Proxy(Proxy))
import Data.Type.Equality (type (==))

type I = "i" :: DimensionName
type J = "j" :: DimensionName
type K = "k" :: DimensionName

type I_1_9 = Dim I '(1, 9)
type I_2_9 = Dim I '(2, 9)
type I_2_11 = Dim I '(2, 11)
type J_5_8 = Dim J '(5, 8)
type K_0_7 = Dim K '(0, 7)

type I_1_9_J_5_8 = '[I_1_9, J_5_8]
type I_2_11_K_0_7 = '[I_2_11, K_0_7]
type I_2_9_J_5_8_K_0_7 = '[I_2_9, J_5_8, K_0_7]

i = Proxy @I
j = Proxy @J
k = Proxy @K

dimensionSetTests = [
        testGroup "dimensionSet" [
            testCase "Intersect" $ Proxy @(Intersect I_1_9 I_2_11) @?= Proxy @(Dim I '(2, 9)),
            testCase "MaybeIntersect 1" $ Proxy @(MaybeIntersect (Just I_1_9) (Just I_2_11)) @?= Proxy @(Dim I '(2, 9)),
            testCase "MaybeIntersect 2" $ Proxy @(MaybeIntersect (Just I_1_9) Nothing) @?= Proxy @I_1_9,
            testCase "MaybeIntersect 3" $ Proxy @(MaybeIntersect Nothing (Just I_2_11)) @?= Proxy @I_2_11,
            testCase "Delete" $ Proxy @(Delete 2 '[1, 2, 3]) @?= Proxy @'[1, 3],
            testCase "Union" $ Proxy @(Union '[1, 2, 3] '[1, 3, 4, 5]) @?= Proxy @'[1, 2, 3, 4, 5],
            testCase "Names" $ Proxy @(Names I_2_11_K_0_7) @?= Proxy @'[I, K],
            testCase "GetDim 1" $ Proxy @(GetDim I I_2_11_K_0_7) @?= Proxy @(Just I_2_11),
            testCase "GetDim 2" $ Proxy @(GetDim J I_2_11_K_0_7) @?= Proxy @Nothing,
            testCase "GetDim 3" $ Proxy @(GetDim K I_2_11_K_0_7) @?= Proxy @(Just K_0_7),
            testCase "DimIndex 1" $ Proxy @(DimIndex I I_2_11_K_0_7) @?= Proxy @0,
            testCase "DimIndex 2" $ Proxy @(DimIndex K I_2_11_K_0_7) @?= Proxy @1,
            testCase "dimIndex 1" $ (dimIndex i (Proxy @I_2_11_K_0_7)) @?= 0,
            testCase "dimIndex 2" $ (dimIndex k (Proxy @I_2_11_K_0_7)) @?= 1,
            testCase "DimIndices" $ Proxy @(DimIndices '[I, K] I_2_11_K_0_7) @?= Proxy @'[0, 1],
            testCase "dimIndices" $ dimIndices (Proxy @'[I, K]) (Proxy @I_2_11_K_0_7) @?= [0, 1],
            testCase "UDIR" $ Proxy @(UDIR I_1_9_J_5_8 I_2_11_K_0_7) @?= Proxy @I_2_9_J_5_8_K_0_7,
            testCase "Index" $ index (Index @I_1_9_J_5_8 [2, 7]) @?= [2, 7],
            testCase "indexAtDim" $ indexAtDim j (Index @I_1_9_J_5_8 [2, 7]) @?= 7,
            testCase "shiftIndexTo" $ shiftIndexTo j 5 (Index @I_1_9_J_5_8 [2, 7]) @?= Index @I_1_9_J_5_8 [2, 5],
            testCase "shiftIndexBy" $ shiftIndexBy i 2 (Index @I_1_9_J_5_8 [2, 7]) @?= Index @I_1_9_J_5_8 [4, 7]
            --testCase "extendIndex" $ extendIndex (Proxy @I_2_9_J_5_8_K_0_7) (Index @I_2_11_K_0_7 [2, 5]) @?= Index @I_2_9_J_5_8_K_0_7 [2, 0, 5]
        ]
    ]
