module Main (main) where

import Test.Framework

import IteratorIr

import CartesianTests
import ToyConnectivityTests
import ToyConnectivity2DTests

tests = cartesianTests ++ toyConnectivityTests ++ toyConnectivity2DTests

main :: IO()
main = defaultMain tests
