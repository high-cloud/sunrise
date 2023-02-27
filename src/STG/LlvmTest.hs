{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module STG.LlvmTest where


import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Constant as AST

import qualified LLVM.IRBuilder.Module as L
import qualified LLVM.IRBuilder.Monad as L
import qualified LLVM.IRBuilder.Instruction as L
import qualified LLVM.IRBuilder.Constant as L
import LLVM.Prelude (ShortByteString)
import LLVM.IRBuilder (IRBuilder)
import qualified LLVM.AST.Global as L

pointerLength = AST.i32

test1 :: L.ModuleBuilder ()
test1 = do
  spa <- L.global (AST.Name "Spa") (AST.ArrayType 100 AST.i32) (AST.Int 32 0)
  return ()


