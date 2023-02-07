module ToyConnectivity2DTests (
    toyConnectivity2DTests
) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import IteratorIr

import qualified ToyConnectivity as TC

data Index a = Index { hIndex :: a, vIndex :: Int } deriving Eq

sV o (Index h v) = Index h (v + o)

c2e (Index h v) = [Index hIdx v | hIdx <- (TC.c2e h)]
v2v (Index h v) = [Index hIdx v | hIdx <- (TC.v2v h)]
e2v (Index h v) = [Index hIdx v | hIdx <- (TC.e2v h)]
v2e (Index h v) = [Index hIdx v | hIdx <- (TC.v2e h)]

stencil2 x y = (deref x) + (deref y)

scalarProd = (sum .) . zipWith (*)


edgesToVertices inp = [
        deref $ shift (nb v2e 0) inp,
        deref $ shift (nb v2e 1) inp,
        deref $ shift (nb v2e 2) inp,
        deref $ shift (nb v2e 3) inp
    ]

vItOnV = It (TC.vIdx . hIndex) (Index (TC.Vertex 0) 0)
eItOnV = It (TC.eIdx . hIndex) (Index (TC.Vertex 0) 0)

toyConnectivity2DTests = [
        testGroup "toyConnectivity2D" [
            testCase "deref" $ deref vItOnV @?= 0,
            testCase "shift" $ deref (shift (nb v2v 0) vItOnV) @?= 1,
            testCase "lift1" $ deref (lift1 deref vItOnV) @?= 0,
            testCase "lift1 + shift" $ deref (shift (nb v2v 0) (lift1 deref vItOnV)) @?= 1,
            testCase "lift2" $ deref (lift2 stencil2 vItOnV vItOnV) @?= 0,
            testCase "lift2 + shift" $ deref (shift (nb v2v 0) (lift2 stencil2 vItOnV vItOnV)) @?= 2,
            testCase "nbShift v2v" $ deref (nbShift v2v vItOnV) @?= [1, 3, 2, 6],
            testCase "nbShift v2v + shift" $ deref (shift (nb v2v 0) (nbShift v2v vItOnV)) @?= [2, 3, 0, 7],
            testCase "edgesToVertices" $ edgesToVertices eItOnV @?= [0, 15, 2, 9],
            testCase "nbShift v2e" $ deref (nbShift v2e eItOnV) @?= [0, 15, 2, 9],
            testCase "neighbor sum" $ reduce1 (+) 0 (nbShift v2e eItOnV) @?= 26,
            testCase "lifted + shifted neighbor sum" $ deref (shift (nb v2v 0) (lift1 (reduce1 (+) 0) (nbShift v2e eItOnV))) @?= 27,
            testCase "multi-sum" $ reduce2 (\a x y -> a + x * y) 0 (nbShift v2v vItOnV) (nbShift v2e eItOnV) @?= 103,
            testCase "multi-sum without reduce" $ (scalarProd (derefedNbShift v2v vItOnV) (derefedNbShift v2e eItOnV)) @?= 103
        ]
    ]
