{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import LLVM.IRBuilder (buildModule)
import STG.LlvmTest (test1)

someFunc :: IO ()
someFunc = print $ buildModule "test1" test1
