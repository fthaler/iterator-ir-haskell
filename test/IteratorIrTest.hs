module Main (main) where

import Test.Framework

import IteratorIr

import CartesianTests
import DimensionSetTests
import ToyConnectivityTests
import ToyConnectivity2DTests
import ToyConnectivityNestedTests

tests = cartesianTests ++ dimensionSetTests ++ toyConnectivityTests ++ toyConnectivity2DTests ++ toyConnectivityNestedTests

main :: IO()
main = defaultMain tests
