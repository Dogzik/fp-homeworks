module Structure
  ( Arg
  , ArgFragment(..)
  , Assignment(..)
  , Command(..)
  , DollarExpr(..)
  , ElifClause(..)
  , ElseClause(..)
  , IfClause(..)
  , Program
  , SingleCommand(..)
  , WhileClause(..)
  ) where

data DollarExpr
  = PosArg Int
  | EnvVar String
  | InlineCall Program
  deriving (Show)

data ArgFragment
  = Symbols String
  | Symbol Char
  | Expr DollarExpr
  deriving (Show)

type Arg = [ArgFragment]

data Assignment = Assignment
  { key   :: String
  , value :: Arg
  } deriving (Show)

data SingleCommand = SingleCommand
  { name :: Arg
  , args :: [Arg]
  } deriving (Show)

data ElifClause = ElifClause
  { elifCond :: [Command]
  , elifBody :: [Command]
  } deriving (Show)

newtype ElseClause = ElseClause
  { elseBody :: [Command]
  } deriving (Show)

data IfClause = IfClause
  { ifCond      :: [Command]
  , ifBody      :: [Command]
  , elifClauses :: [ElifClause]
  , elseClause  :: Maybe ElseClause
  } deriving (Show)

data WhileClause = WhileClause
  { whileCond :: [Command]
  , whileBode :: [Command]
  } deriving (Show)

data Command
  = SimpleCommand SingleCommand
  | AssignCommand Assignment
  | If IfClause
  | While WhileClause
  | Subshell Program
  deriving (Show)

type Program = [Command]
