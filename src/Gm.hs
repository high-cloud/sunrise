module Gm where

import CommonDef

type GmState =
  ( GmCode
  , GmStack
  , GmHeap
  , GmGlobals
  , GmStats)


type GmCode = [Instuction]
getType :: GmState ->GmCode
getType (i, _,_,_,_,_) = i


data Instuction
  = Unwind
  | PushGlobal Name
  | PushInt Int
  | Push Int
  | Mkap
  | Slide Int
  | Update Int
  | Pop Int
  deriving (Eq, Show)