module Main (main) where

import Test.Framework

import IteratorIr

import ToyConnectivityTests

tests = toyConnectivityTests

main :: IO()
main = defaultMain tests
