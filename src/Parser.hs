module Parser
  ( parseSourceFile,
    parseFiles,
    SourceFile (..),
    Expr (..),
    Literal (..),
    ClassDecl (..),
    ClassItem (..),
    VarDecl (..),
    FnDecl (..),
    TypeName (..),
    ExecItem (..),
  )
where

import Control.Monad (void)
import Data.Functor (($>))
import Data.Maybe (isJust)
import Text.Parsec
  ( alphaNum,
    anyChar,
    between,
    char,
    choice,
    digit,
    eof,
    letter,
    many,
    many1,
    noneOf,
    optionMaybe,
    sepBy,
    sepBy1,
    skipMany,
    space,
    string,
    try,
    (<?>),
    (<|>),
  )
import Text.Parsec.Prim (runParser)
import Text.Parsec.String (Parser)

data SourceFile = SourceFile
  { package :: Maybe [String],
    imports :: [[String]],
    classes :: [ClassDecl]
  }
  deriving (Show)

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
  | NotOp Expr
  | InstanceOf Expr Expr
  | SubscriptOp Expr Expr
  | DotOp Expr String
  | -- This also includes casts
    CallOp Expr [Expr]
  | LiteralExpr Literal
  | VarExpr String
  | -- Here the Expr is just the length of the array; contents can't
    -- be initialized in the declaration.
    NewArray TypeName Expr
  | NewClass TypeName [Expr]
  deriving (Show)

data Literal
  = IntLit Integer
  | RealLit Double
  | CharLit Char
  | StringLit String
  deriving (Show)

data ClassDecl = ClassDecl
  { clsPublic :: Bool,
    clsStatic :: Bool,
    clsName :: String,
    clsMembers :: [ClassItem]
  }
  deriving (Show)

-- TODO: allow constructors
data ClassItem
  = ClassVarDecl VarDecl
  | ClassFnDecl FnDecl
  deriving (Show)

-- TODO: names can also be arrays (brackets on the right).
data VarDecl = VarDecl
  { varPublic :: Bool,
    varStatic :: Bool,
    varType :: TypeName,
    varNames :: [(String, Maybe Expr)]
  }
  deriving (Show)

data FnDecl = FnDecl
  { fnPublic :: Bool,
    fnStatic :: Bool,
    fnRetType :: TypeName,
    fnName :: String,
    fnParams :: [(TypeName, String)],
    -- fnBody can be the name of a symbol for an "extern" function, or
    -- the body of the function.
    fnBody :: Either String [ExecItem]
  }
  deriving (Show)

data TypeName
  = BuiltinTypeName String
  | ClassTypeName [String]
  | ArrayTypeName TypeName
  deriving (Show)

-- TODO: Switch statements.
data ExecItem
  = Block [ExecItem]
  | ExecVarDecl VarDecl
  | ExecEval Expr
  | RetStmt Expr
  | IfStmt
      { ifCondition :: Expr,
        ifBody :: ExecItem,
        elseBody :: Maybe ExecItem
      }
  | WhileStmt
      { whileCondition :: Expr,
        whileBody :: ExecItem
      }
  | ForStmt
      { forInit :: Expr,
        forCondition :: Expr,
        forIncrement :: Expr,
        forBody :: ExecItem
      }
  deriving (Show)

reservedWords :: [String]
reservedWords =
  [ "boolean",
    "break",
    "case",
    "char",
    "class",
    "continue",
    "default",
    "double",
    "else",
    "float",
    "for",
    "if",
    "instanceof",
    "int",
    "long",
    "new",
    "public",
    "return",
    "static",
    "switch",
    "void",
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

-- Basic Parsec setup
lexeme :: Parser a -> Parser a
lexeme = (<* skipMany space)

word :: String -> Parser String
word = lexeme . string

-- Parse a string literal.
parseString :: Parser Literal
parseString =
  lexeme
    ( between
        (char '"')
        (char '"')
        (StringLit <$> many stringChar)
        <?> "string literal"
    )

-- Parse a character literal.
parseChar :: Parser Literal
parseChar =
  lexeme
    ( between
        (char '\'')
        (char '\'')
        (CharLit <$> stringChar)
    )
    <?> "char literal"

stringChar = noneOf ['\\', '\"'] <|> char '\\' *> (backslash <$> anyChar)

-- TODO: octal, hex & Unicode escapes.
backslash c = case c of
  'a' -> '\x07'
  'b' -> '\x08'
  'f' -> '\x0C'
  'n' -> '\x0A'
  'r' -> '\x0D'
  't' -> '\x09'
  'v' -> '\x0B'
  c -> c

-- Parse a number, i.e., either an integer or a decimal.
parseNumber :: Parser Literal
parseNumber =
  lexeme
    ( do
        leading <- many1 digit
        choice
          [ do
              trailing <- char '.' *> many1 digit
              pure $ RealLit (read (leading ++ "." ++ trailing)),
            pure $ IntLit (read leading)
          ]
    )
    <?> "number literal"

-- Parse any literal.
parseLiteral :: Parser Expr
parseLiteral =
  LiteralExpr
    <$> choice
      [ parseString,
        parseChar,
        parseNumber
      ]

-- Parse a possible variable name.
parseIdent :: Parser String
parseIdent =
  lexeme
    -- We use `try` here to prevent reading reserved words.
    ( try $ do
        -- Variable names have to start with a letter.
        word <- (:) <$> letter <*> many alphaNum
        if word `elem` reservedWords
          then fail ("got reserved word `" ++ word ++ "`")
          else pure word
    )
    <?> "variable name"

-- Parse a variable literal.
parseVariable :: Parser Expr
parseVariable = VarExpr <$> parseIdent

-- An operator, i.e., a single character optionally surrounded by
-- whitespace.
sym :: Char -> Parser ()
sym = lexeme . void . char

optionBool :: Parser a -> Parser Bool
optionBool p = isJust <$> optionMaybe p

parseBlock :: Parser ExecItem
parseBlock =
  Block <$> between (sym '{') (sym '}') (many parseExecItem)

parseVarDeclStmt :: Parser ExecItem
parseVarDeclStmt = ExecVarDecl <$> parseVarDecl

parseEvalStmt :: Parser ExecItem
parseEvalStmt = ExecEval <$> parseExpr <* sym ';'

parseReturnStmt :: Parser ExecItem
parseReturnStmt =
  word "return" *> (RetStmt <$> parseExpr) <* sym ';'

parseIfStmt :: Parser ExecItem
parseIfStmt =
  IfStmt
    <$> ( word "if"
            *> between (sym '(') (sym ')') parseExpr
        )
    <*> parseExecItem
    <*> optionMaybe (word "else" *> parseExecItem)

parseWhileStmt :: Parser ExecItem
parseWhileStmt =
  WhileStmt
    <$> ( word "while"
            *> between (sym '(') (sym ')') parseExpr
        )
    <*> parseExecItem

parseForStmt :: Parser ExecItem
parseForStmt =
  ForStmt
    <$> (word "for" *> sym '(' *> parseExpr <* sym ';')
    <*> (parseExpr <* sym ';')
    <*> (parseExpr <* sym ')')
    <*> parseExecItem

parseExecItem :: Parser ExecItem
parseExecItem =
  choice
    [ try parseVarDeclStmt,
      parseEvalStmt,
      parseBlock,
      parseReturnStmt,
      parseIfStmt,
      parseWhileStmt,
      parseForStmt
    ]

parseVarDecl :: Parser VarDecl
parseVarDecl =
  VarDecl
    <$> optionBool (word "public")
    <*> optionBool (word "static")
    <*> parseTypeName
    <*> ( (,)
            <$> parseIdent
            <*> optionMaybe (sym '=' *> parseExpr)
        )
      `sepBy1` sym ','
      <* sym ';'

parseFnArg :: Parser (TypeName, String)
parseFnArg =
  (,)
    <$> parseTypeName
    <*> parseIdent

parseFnDecl :: Parser FnDecl
parseFnDecl =
  FnDecl
    <$> optionBool (word "public")
    <*> optionBool (word "static")
    <*> parseTypeName
    <*> parseIdent
    <*> between
      (sym '(')
      (sym ')')
      ( ( (,)
            <$> parseTypeName
            <*> parseIdent
        )
          `sepBy` sym ','
      )
    <*> choice
      [ Right <$> between (sym '{') (sym '}') (many parseExecItem),
        Left <$> (word "extern" *> parseIdent <* sym ';')
      ]

parseClassItem :: Parser ClassItem
parseClassItem =
  choice
    [ try $ ClassVarDecl <$> parseVarDecl,
      ClassFnDecl <$> parseFnDecl
    ]

parseClassDecl :: Parser ClassDecl
parseClassDecl =
  ClassDecl
    <$> optionBool (word "public")
    <*> optionBool (word "static")
    <*> (word "class" *> parseIdent)
    <*> between (sym '{') (sym '}') (many parseClassItem)

parseBuiltinType :: Parser TypeName
parseBuiltinType =
  choice
    ( map
        (\tname -> word tname $> BuiltinTypeName tname)
        primTypes
    )

parseClassType :: Parser TypeName
parseClassType = ClassTypeName <$> parseIdent `sepBy` sym '.'

parseTypeName :: Parser TypeName
parseTypeName = do
  base <-
    choice
      [ parseBuiltinType,
        parseClassType
      ]
  -- N.B. Parsing array types directly would be left-recursive; we
  -- instead parse them here as suffixes.
  arrSuffixes <- many (sym '[' <* sym ']')
  pure $ foldl (const . ArrayTypeName) base arrSuffixes

data Assoc = AssocLeft | AssocRight

-- Parse a list of infix operations between elements. All the infix
-- operations are of the same precedence level and associativity.
parseInfixes ::
  -- | Parsers for the operators, which return the operator mapping: a
  -- | function that takes two expressions (left- and right-hand side,
  -- | respectively) and returns a new expression.
  [Parser (Expr -> Expr -> Expr)] ->
  -- | Associativity of the operation.
  Assoc ->
  -- | The constituent elements in the infix expression.
  Parser Expr ->
  Parser Expr
parseInfixes infixes assoc element = do
  first <- element
  rest <- many ((,) <$> choice infixes <*> element)
  pure $
    ( case assoc of
        AssocLeft -> foldl (\l (operator, r) -> operator l r)
        -- BUG: AssocRight generates wildly incorrect results
        AssocRight -> foldr (\(operator, r) l -> operator l r)
    )
      first
      rest

-- Parse a list of prefixes that can precede an expression.
parsePrefixes ::
  -- | The list of prefixes. Each prefix maps an expression to a new
  -- | expression, so is of type Expr -> Expr.
  [Parser (Expr -> Expr)] ->
  -- | The expression parser that can be preceded by prefixes.
  Parser Expr ->
  Parser Expr
parsePrefixes prefixes element = do
  p <- many (choice prefixes)
  e <- element
  pure $ foldl (\expression mapping -> mapping expression) e p

-- Parse a list of suffixes that can precede an expression.
parseSuffixes ::
  -- | The list of suffixes. Each suffix maps an expression to a new
  -- | expression, so is of type Expr -> Expr.
  [Parser (Expr -> Expr)] ->
  -- | The expression parser that can be preceded by prefixes.
  Parser Expr ->
  Parser Expr
parseSuffixes suffixes element = do
  e <- element
  p <- many (choice suffixes)
  pure $ foldl (\expression mapping -> mapping expression) e p

parseConstruction :: Parser Expr
parseConstruction =
  NewClass
    <$> (word "new" *> parseTypeName)
    <*> between
      (sym '(')
      (sym ')')
      (parseExpr `sepBy` sym ',')

-- The base expression parser, which includes literals (1, 2.5,
-- "hello"), variables (print, x), parentheses, and constructors.
expBase :: Parser Expr
expBase =
  choice
    [ parseLiteral,
      parseVariable,
      between (sym '(') (sym ')') parseExpr,
      parseConstruction
    ]

eGroupAssignment :: Parser Expr -> Parser Expr
eGroupAssignment =
  parseInfixes
    [ EqOp <$ sym '='
    ]
    AssocLeft

eGroupAddSub :: Parser Expr -> Parser Expr
eGroupAddSub =
  parseInfixes
    [ AddOp <$ sym '+',
      SubOp <$ sym '-'
    ]
    AssocLeft

eGroupMulDiv :: Parser Expr -> Parser Expr
eGroupMulDiv =
  parseInfixes
    [ MulOp <$ sym '*',
      DivOp <$ sym '/',
      ModOp <$ sym '%'
    ]
    AssocLeft

eGroupIncDec :: Parser Expr -> Parser Expr
eGroupIncDec =
  parseSuffixes
    -- These are prefixes of the + and - operators, therefore need
    -- lookahead.
    [ try $ Increment <$ word "++",
      try $ Decrement <$ word "--"
    ]

eGroupNegate :: Parser Expr -> Parser Expr
eGroupNegate =
  parsePrefixes
    [ Negate <$ sym '-'
    ]

eGroupCompare :: Parser Expr -> Parser Expr
eGroupCompare =
  parseInfixes
    -- Use lookahead for operations that are prefixes of others.
    [ try $ EqOp <$ word "==",
      NeqOp <$ word "!=",
      try $ GteOp <$ word ">=",
      try $ LteOp <$ word "<=",
      GtOp <$ word ">",
      LtOp <$ word "<"
    ]
    AssocLeft

eGroupAnd :: Parser Expr -> Parser Expr
eGroupAnd =
  parseInfixes
    [ AndOp <$ word "&&"
    ]
    AssocLeft

eGroupOr :: Parser Expr -> Parser Expr
eGroupOr =
  parseInfixes
    [ OrOp <$ word "||"
    ]
    AssocLeft

eGroupNot :: Parser Expr -> Parser Expr
eGroupNot =
  parsePrefixes
    [ NotOp <$ sym '!'
    ]

eGroupInstanceOf :: Parser Expr -> Parser Expr
eGroupInstanceOf =
  parseInfixes
    [ InstanceOf <$ word "instanceof"
    ]
    AssocLeft

eGroupSubscripts :: Parser Expr -> Parser Expr
eGroupSubscripts base = do
  first <- base
  subscripts <-
    many $
      choice
        [ between (sym '[') (sym ']') (Left <$> parseExpr),
          sym '.' *> (Right <$> parseIdent)
        ]
  pure $
    foldl
      ( \expr suffix -> case suffix of
          Left bracketed -> SubscriptOp expr bracketed
          Right dotted -> DotOp expr dotted
      )
      first
      subscripts

eGroupCalls :: Parser Expr -> Parser Expr
eGroupCalls base = do
  first <- base
  calls <-
    many $
      between
        (sym '(')
        (sym ')')
        (parseExpr `sepBy` sym ',')
  pure $ foldl CallOp first calls

parseExpr :: Parser Expr
parseExpr =
  -- Fold all the smaller expression parsers into one big parser.
  foldl
    (\e p -> p e)
    expBase
    [ -- "x"[y], new Foo().y -> (new Foo()).y
      eGroupSubscripts,
      -- foo.bar(baz) -> (foo.bar)(baz)
      eGroupCalls,
      -- x()++ -> (x())++
      eGroupIncDec,
      -- x++ instanceof y -> (x++) instanceof y
      eGroupInstanceOf,
      -- -x instanceof y -> -(x instanceof y)
      eGroupNegate,
      -- !-x -> !(-x) (these should prob be in the same group honestly)
      eGroupNot,
      -- !x * !y -> (!x) * (!y)
      eGroupMulDiv,
      -- a * b + c * d -> (a * b) + (c * d)
      eGroupAddSub,
      -- a + b == c + d -> (a + b) == (c + d)
      eGroupCompare,
      -- a == b && c == d -> (a == b) && (c == d)
      eGroupAnd,
      -- a && b || c && d -> (a && b) || (c && d)
      eGroupOr,
      -- a || b = c || d -> (a || b) = (c || d)
      eGroupAssignment
    ]

parsePackage :: Parser [String]
parsePackage =
  -- This `try` is a nasty hack to make `package` not collide with
  -- `public class`. It makes me sad :c
  try (word "package") *> parseIdent `sepBy1` sym '.' <* sym ';'

parseImport :: Parser [String]
parseImport =
  word "import" *> parseIdent `sepBy1` sym '.' <* sym ';'

parseSourceFile :: Parser SourceFile
parseSourceFile =
  SourceFile
    <$> (many space *> optionMaybe parsePackage)
    <*> many parseImport
    <*> many parseClassDecl
    <* eof

parseFiles :: [String] -> IO (Either String [SourceFile])
parseFiles files = do
  texts <- sequence (readFile <$> files)
  let results = zipWith (runParser parseSourceFile ()) files texts
      firstErr = sequence results
  pure $ case firstErr of
    Left err -> Left (show err)
    Right succ -> Right succ
