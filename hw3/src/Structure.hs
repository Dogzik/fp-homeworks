module Structure
  ( Arg
  , ArgFragment
  , Command
  , DollarExpr(..)
  , ElifClause
  , ElseClause
  , IfClause
  , Program
  , SingleCommand
  , WhileClause
  ) where

data DollarExpr
  = PosArg Int
  | EnvVar String
  | InlineCall Program
  deriving (Show)

data ArgFragment
  = Symbols String
  | Expr DollarExpr
  deriving (Show)

type Arg = [ArgFragment]

data SingleCommand = SingleCommand
  { name :: String
  , args :: [Arg]
  } deriving (Show)

data ElifClause = ElifClause
  { elifCond :: [SingleCommand]
  , elifBody :: [Command]
  } deriving (Show)

newtype ElseClause = ElseClause
  { elseBody :: [Command]
  } deriving (Show)

data IfClause = IfClause
  { ifCond      :: [SingleCommand]
  , ifBody      :: [Command]
  , elifClauses :: [ElifClause]
  , elseClause  :: ElseClause
  } deriving (Show)

data WhileClause = WhileClause
  { whileCond :: [SingleCommand]
  , whileBode :: [Command]
  } deriving (Show)

data Command
  = SimpleCommand SingleCommand
  | If IfClause
  | While WhileClause
  | Subshell Program
  deriving (Show)

type Program = [Command]
