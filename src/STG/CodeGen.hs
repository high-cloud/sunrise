{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module STG.CodeGen where

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST

import qualified LLVM.IRBuilder.Module as L
import qualified LLVM.IRBuilder.Monad as L
import qualified LLVM.IRBuilder.Instruction as L
import qualified LLVM.IRBuilder.Constant as L
import LLVM.Prelude (ShortByteString)

import qualified Data.Map as M
import Control.Monad.State
import Data.String

import STG.Syntax
import LLVM.IRBuilder (IRBuilder)
import Data.Text (Text)

data CodeGenEnv = CodeGenEnv { operands :: M.Map Text AST.Operand}
type LLVM = L.ModuleBuilderT (State CodeGenEnv)
type CodeGen = L.IRBuilderT LLVM

type PointerLen = AST.i64

-- | allocate basic stacks, heap and registers
initGM :: L.ModuleBuilder ()
initGM =
  spa_lim = L.global "SpA"
  -- spa <- L.global 'SpA" AST.