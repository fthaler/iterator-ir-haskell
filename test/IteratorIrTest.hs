module Main (main) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import IteratorIr

import ToyConnectivity


stencil2 x y = (deref x) + (deref y)

scalarProd = (sum .) . zipWith (*)


edgesToVertices inp = [
        deref $ shift (nb v2e 0) inp,
        deref $ shift (nb v2e 1) inp,
        deref $ shift (nb v2e 2) inp,
        deref $ shift (nb v2e 3) inp
    ]

vItOnV = It vIdx (Vertex 0)
eItOnV = It eIdx (Vertex 0)

tests = [
        testGroup "basic" [
            testCase "deref" $ deref vItOnV @?= 0,
            testCase "shift" $ deref (shift (nb v2v 0) vItOnV) @?= 1,
            testCase "lift1" $ deref (lift1 deref vItOnV) @?= 0,
            testCase "lift1 + shift" $ deref (shift (nb v2v 0) (lift1 deref vItOnV)) @?= 1,
            testCase "lift2" $ deref (lift2 stencil2 vItOnV vItOnV) @?= 0,
            testCase "lift2 + shift" $ deref (shift (nb v2v 0) (lift2 stencil2 vItOnV vItOnV)) @?= 2,
            testCase "nbShift" $ deref (nbShift v2v vItOnV) @?= [1, 3, 2, 6],
            testCase "nbShift" $ deref (shift (nb v2v 0) (nbShift v2v vItOnV)) @?= [2, 3, 0, 7]
        ],
        testGroup "edgesToVertices" [
            testCase "basic" $ edgesToVertices eItOnV @?= [0, 15, 2, 9],
            testCase "nbShift" $ deref (nbShift v2e eItOnV) @?= [0, 15, 2, 9],
            testCase "neighbor sum" $ reduce1 (+) 0 (nbShift v2e eItOnV) @?= 26,
            testCase "lifted + shifted neighbor sum" $ deref (shift (nb v2v 0) (lift1 (reduce1 (+) 0) (nbShift v2e eItOnV))) @?= 27
        ],
        testGroup "complex" [
            testCase "multi-sum" $ reduce2 (\a x y -> a + x * y) 0 (nbShift v2v vItOnV) (nbShift v2e eItOnV) @?= 103,
            testCase "multi-sum without reduce" $ (scalarProd (derefedNbShift v2v vItOnV) (derefedNbShift v2e eItOnV)) @?= 103
        ]
    ]

main :: IO()
main = defaultMain tests
