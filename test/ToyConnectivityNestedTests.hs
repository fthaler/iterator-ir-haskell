module ToyConnectivityNestedTests (
    toyConnectivityNestedTests
) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Control.Lens.Internal.Context

import IteratorIr
import ToyConnectivity

stencil2 x y = deref x + deref y

scalarProd = (sum .) . zipWith (*)


edgesToVertices inp = [
        deref $ shift (nb v2e 0) inp,
        deref $ shift (nb v2e 1) inp,
        deref $ shift (nb v2e 2) inp,
        deref $ shift (nb v2e 3) inp
    ]

vItOnV = It vIdx (Vertex 0)
eItOnV = It eIdx (Vertex 0)
vItOnE = It vIdx (Edge 1)
eItOnE = It eIdx (Edge 0)

toyConnectivityNestedTests = [
        testGroup "toyConnectivityNestedTests" [
            testCase "neighborhood" $ fmap deref (neighborhood e2v vItOnE) @?= [1, 2],
            testCase "lifted neighborhood" $ let liftedN = lift (neighborhood e2v) vItOnE
                                             in (ipos liftedN, fmap deref (deref liftedN)) @?= (Edge 1, [1, 2]),
            testCase "reduce lifted neighborhood" $ reduce1 (\accum it -> accum + deref it) 0 (lift (neighborhood e2v) vItOnE) @?= 3
        ]
    ]
