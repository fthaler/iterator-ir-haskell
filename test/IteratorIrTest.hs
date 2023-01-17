module Main (main) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import IteratorIr

import ToyConnectivity as TC


stencil2 x y = (deref x) + (deref y)


edgesToVertices inp = [
        deref $ shift (TC.v2e 0) inp,
        deref $ shift (TC.v2e 1) inp,
        deref $ shift (TC.v2e 2) inp,
        deref $ shift (TC.v2e 3) inp
    ]

vItOnV = It TC.vIdx (TC.Vertex 0)
eItOnV = It TC.eIdx (TC.Vertex 0)

tests = [
        testGroup "basic" [
            testCase "deref" $ deref vItOnV @?= 0,
            testCase "shift" $ deref (shift (TC.v2v 0) vItOnV) @?= 1,
            testCase "lift1" $ deref (lift1 deref vItOnV) @?= 0,
            testCase "lift1 + shift" $ deref (shift (TC.v2v 0) (lift1 deref vItOnV)) @?= 1,
            testCase "lift2" $ deref (lift2 stencil2 vItOnV vItOnV) @?= 0,
            testCase "lift2 + shift" $ deref (shift (TC.v2v 0) (lift2 stencil2 vItOnV vItOnV)) @?= 2,
            testCase "nbShift 2" $ deref (nbShift 2 TC.v2v vItOnV) @?= [1, 3],
            testCase "nbShift 4" $ deref (nbShift 4 TC.v2v vItOnV) @?= [1, 3, 2, 6],
            testCase "nbShift 4" $ deref (shift (TC.v2v 0) (nbShift 4 TC.v2v vItOnV)) @?= [2, 3, 0, 7]
        ],
        testGroup "edgesToVertices" [
            testCase "basic" $ edgesToVertices eItOnV @?= [0, 15, 2, 9],
            testCase "nbShift" $ deref (nbShift 4 TC.v2e eItOnV) @?= [0, 15, 2, 9],
            testCase "neighbor sum" $ reduce1 (+) 0 (nbShift 4 TC.v2e eItOnV) @?= 26,
            testCase "lifted neighbor sum" $ deref (lift1 (reduce1 (+) 0) (nbShift 4 TC.v2e eItOnV)) @?= 26
        ]
    ]

main :: IO()
main = defaultMain tests
