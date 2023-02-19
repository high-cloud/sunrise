module STG.Syntax where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as M
import Data.Map(Map)
import Data.Text (Text)

newtype Program = Progarm Binds

newtype Binds = Binds (Map StgVar LambdaForm)



data LambdaForm
  = LambdaForm
      ![StgVar]
      !UpdateFlag -- ^ whether updatable
      ![StgVar] -- local variabel
      !StgExpr

data UpdateFlag = Updata | NoUpdate


data Rec = NonRecursive | Recursive
data StgExpr
  = StgLet !Rec !Binds !StgExpr
  | StgCase !StgExpr !StgAlts
  | StgAppF !StgVar ![StgAtom]
  | StgAppC !StgConstr ![StgAtom] -- ^ Saturated constructor application
  | StgAppP !PrimOp !StgAtom !StgAtom -- ^ primitive function application. e.g. *# 1# 2#
  | StgLitE !Literal

data StgAtom
  = StgAtomVar !StgVar
  | StgAtomLit !Literal

type Literal = Int

data StgAlts = StgAlts !NonDefaultAlts !DefaultAlt

data NonDefaultAlts
  = NoNonDefaultAlts
  |  AdtAlts !(NonEmpty AdtAlt)
  | PrimAlts !(NonEmpty PrimAlt)

data AdtAlt = AdtAlt !StgConstr ![StgVar] !StgExpr

data PrimAlt = PrimAlt !Literal !StgExpr

data DefaultAlt
  = DefaultNotBound !StgExpr
  | DefaultBound !StgVar !StgExpr

data PrimOp
  = Add
  | Sub

newtype StgVar = StgVar Text

newtype StgConstr = ConstrText Text