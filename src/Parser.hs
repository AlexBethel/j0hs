module Parser () where

import Text.Parsec.String (Parser)

data SourceFile = SourceFile
  { imports :: [Expr],
    classes :: [ClassDecl]
  }

data Expr
  = Assignment Expr Expr
  | AddOp Expr Expr
  | SubOp Expr Expr
  | MulOp Expr Expr
  | DivOp Expr Expr
  | ModOp Expr Expr
  | Increment Expr
  | Decrement Expr
  | Negate Expr
  | EqOp Expr Expr
  | NeqOp Expr Expr
  | GtOp Expr Expr
  | LtOp Expr Expr
  | GteOp Expr Expr
  | LteOp Expr Expr
  | AndOp Expr Expr
  | OrOp Expr Expr
  | InstanceOf Expr Expr
  | SubscriptOp Expr Expr
  | DotOp Expr Expr
  | -- This also includes casts
    CallOp Expr [Expr]
  | LiteralExpr Literal
  | VarExpr String
  | -- Here the Expr is just the length of the array; contents can't
    -- be initialized in the declaration.
    NewArray TypeName Expr
  | NewClass TypeName [Expr]

data Literal
  = IntLit Integer
  | RealLit Double
  | CharLit String
  | StringLit String

data ClassDecl = ClassDecl
  { clsPublic :: Bool,
    clsStatic :: Bool,
    clsName :: String,
    clsMembers :: [ClassItem]
  }

data ClassItem
  = ClassVarDecl VarDecl
  | ClassFnDecl FnDecl

-- TODO: names can also be arrays (brackets on the right).
data VarDecl = VarDecl
  { varPublic :: Bool,
    varStatic :: Bool,
    varType :: TypeName,
    varNames :: [String]
  }

data FnDecl = FnDecl
  { static :: Bool,
    name :: String,
    retType :: TypeName,
    body :: [ExecItem]
  }

data TypeName
  = BuiltinType String
  | ClassType Expr
  | ArrayType TypeName

-- TODO: Switch statements.
data ExecItem
  = ExecVarDecl VarDecl
  | ExecEval Expr
  | RetStmt Expr
  | IfStmt
      { ifCondition :: Expr,
        ifBody :: [ExecItem],
        elseBody :: Maybe [ExecItem]
      }
  | WhileStmt
      { whileCondition :: Expr,
        whileBody :: [ExecItem]
      }
  | ForStmt
      { forInit :: Expr,
        forCondition :: Expr,
        forIncrement :: Expr
      }

reservedWords :: [String]
reservedWords =
  [ "break",
    "case",
    "class",
    "continue",
    "default",
    "else",
    "for",
    "if",
    "instanceof",
    "new",
    "public",
    "return",
    "static",
    "switch",
    "while"
  ]

primTypes :: [String]
primTypes =
  [ "boolean",
    "char",
    "double",
    "float",
    "int",
    "long",
    "void"
  ]

