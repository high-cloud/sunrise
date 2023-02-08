module AST where


data Expr a
  = EVar Name -- Variables
  | ENum Int  -- Numbers
  | EConstr Int Int -- Constructor
  | EAp (Expr a) (Expr a) -- Applications
  | ELet
      IsRec    -- True for recuresive
      [(a, Expr a)]
      (Expr a)
  | ECase           -- case expression
      (Expr a)
      [Alter a]
  | ELam [a] (Expr a)

-- | used in case of
type Alter a = (Int, [a], Expr a)
type CoreAlter = Alter Name

type Name = String
type CoreExpr = Expr Name

type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive = True
nonRecursive = False

bindersOf :: [(a,b)] -> [a]
bindersOf defns = [name | (name,rhs) <- defns]

type Program a = [ScDefns a]
type CoreProgram = Program Name

type ScDefns a = (Name, [a], Expr a)
type CoreScDefn = ScDefns Name