module Main (main) where

import Test.Framework

import IteratorIr

import CartesianTests
import ToyConnectivityTests

tests = cartesianTests ++ toyConnectivityTests

main :: IO()
main = defaultMain tests
