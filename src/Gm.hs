module Gm where

import CommonDef
import AST
import qualified Data.Map as M

type GmState =
  ( GmCode
  , GmStack
  , GmHeap
  , GmGlobals
  , GmStats)


type GmCode = [Instuction]
getType :: GmState ->GmCode
getType (i, _,_,_,_,_) = i

-- | the number of objects in heap, a list of unused addresses, map address to object
type Heap a = (Int, [Int], [(Int,a)])
type GmStack = [Addr]
type GmHeap = Heap Node
type GmGlobals = M.Map Name Addr
type GmStats = Int


data Instuction
  = Unwind
  | PushGlobal Name
  | PushInt Int
  | Push Int
  | Mkap
  | Slide Int
  | Update Int
  | Pop Int
  | Alloc Int -- ^ create n empty graph nodes
  | Eval -- ^ eval to weak head normal form
  | Add | Sub | Mul | Div | Neg
  -- | Eq | Ne | Lt | Le | Gt | Ge
  -- | Cond GmCode GmCode
  deriving (Eq, Show)

type Addr = Int

-- nodes in g-machine
data Node
  = NNum Int
  | NAp Addr Addr
  | NGlobals Int GmCode
  | NInd Addr -- Indirections
  deriving (Show, Eq)


compile :: CoreProgram -> GmState
compile program = (initialCode, [] , heap, globals, statInitial)

initialCode :: GmCode
initialCode = [PushGlobal "main", Unwind]

-- |compiled type
type GmCompiledSC = (Name, Int, GmCode)


-- | allocates a new global for its compiled sc argument, returning the new heap and the address where the global is stored
allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc = _

compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
compileSc (name, env, body) = (name, length env, compileR body (M.fromList $ zip env [0..]))

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

type GmEnvironment = M.Map Name Int
compileR :: GmCompiler
compileR e env = compileC e env ++ [Update d, Pop d, Unwind]
  where d = M.size env

-- | generate code construt thef graph of e
compileC :: CoreExpr -> GmEnvironment ->GmCode
compileC (EVar v) env
  | M.member v env = [Push n]
  | otherwise = [PushGlobal v]
  where n = env M.! v
compileC (ENum n) env = [PushInt n]
compileC (EAp e1 e2) env = compileC e2 env <> compileC e1 (argOffset 1 env) ++ [Mkap]
compileC (ELet recursive defs e) args
  | recursive = compileLetrec compileC defs e args
  | otherwise = compileLet compileC defs e args

compileLet comp defs expr env =
  compileLet' defs env <> comp expr env' <> [Slide (length defs)]
  where
    compileLet' [] env = []
    compileLet' ((name, expr):defs) env = compileC expr env <> compileLet' defs (argOffset 1 env)
    env' = compileArgs defs env

compileLetrec comp defs expr env =
  [Alloc n] <> compileLetrec' defs env' <> comp expr env' <> [Slide n]
  where
    n = length defs
    env' = compileArgs defs env
    compileLetrec' [] env = []
    compileLetrec' ((name, expr):defs) env = comp expr env <> [Update (length defs)] <> compileLetrec' defs env


compileArgs :: [(Name, b)] -> GmEnvironment -> M.Map Name Int
compileArgs defs env = M.fromList (zip (map fst defs) [n-1, n-2 .. 0]) <> argOffset n env
  where n = length defs



argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = fmap go env
  where go m = n+m


