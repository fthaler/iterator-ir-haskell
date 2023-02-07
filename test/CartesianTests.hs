module CartesianTests (
    cartesianTests
) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import IteratorIr

data CartesianIndex = C { iIndex :: Int, jIndex :: Int } deriving Eq

sI o (C i j) = C (i + o) j
sJ o (C i j) = C i (j + o)

inputData idx = (iIndex idx)^3 + 7 * (jIndex idx)^4

initialIterators = [It inputData (C i j) | i <- [0..6], j <- [0..4]]

apply f = map f initialIterators

backwardDiff d x = deref x - deref (shift (d (-1)) x)
forwardDiff d x = backwardDiff d (shift (d 1) x)
centralDiff2 d x = backwardDiff d (lift1 (forwardDiff d) x)
laplacian5Point x = centralDiff2 sI x + centralDiff2 sJ x

cartesianTests = [
    testGroup "cartesian" [
            testCase "deref" $ apply deref @?=
                [inputData (C i j) | i <- [0..6], j <- [0..4]],

            testCase "shift" $ apply (deref . shift (sI 1)) @?= 
                [inputData (C i j) | i <- [1..7], j <- [0..4]],

            testCase "lift1" $ apply (deref . lift1 deref) @?=
                [inputData (C i j) | i <- [0..6], j <- [0..4]],

            testCase "laplacian5Point" $ apply laplacian5Point @?=
                [84 * j^2 + 6 * i + 14 | i <- [0..6], j <- [0..4]]
        ]
    ]
