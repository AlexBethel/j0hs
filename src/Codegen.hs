-- Code generator. This module generates code in x86 assembly for gcc,
-- directly from the AST (no IR is used at the moment). Type checking
-- is done on the fly during code generation.

-- Yes, it's a nasty design, but my goal here is to get a basic
-- compiler working as fast as humanly possible.

-- The specific assembly code we're generating is also very simple and
-- inefficient. When we create a stack-frame, we first allocate memory
-- for each of the named variables used, with each variable taking up
-- a fixed amount of space (in this case, 8 bytes). We then basically
-- use the computer as a stack machine, generating reverse polish
-- notation instruction blocks that operate almost purely on the stack
-- with minimal use of registers. Parameters are passed in the very
-- old-fashioned way, where the caller pushes parameters from right to
-- left, then executes the call, and then the callee returns its
-- return value into the %rax register and the caller cleans up the
-- parameters. The only exception for this is for extern functions, in
-- which special blocks of shim code are generated to convert to the
-- gcc calling convention so standard C functions can be called.

-- The primitive types are all represented as simple blocks of memory
-- from the 8-byte allocation: `boolean` is represented as either a
-- one or a zero followed by seven 00 bytes, `byte` is represented as
-- a 1-byte number followed by 7 zero bytes, `double` is represented
-- as an 8-byte double, `float` is a 4-byte float, `int` is a 4-byte
-- integer, `long` is an 8-byte integer, and `void` is constant
-- zeroes.

-- Class types are represented as pointers to memory allocations
-- managed by `malloc` and `free`, where the first 8 bytes are always
-- a `long` integer representing the current reference count on that
-- object; duplicating references always increases this number by 1,
-- and dropping a reference always decreases the number by 1. As soon
-- as the reference count on an object reaches 0, all of its
-- references are dropped (which may result in recursive object
-- destruction), and the object is then `free`d using the C library
-- routine. This method leaks memory if there are reference cycles,
-- which are quite common in Java. Too bad.

-- Beyond this first 8-byte reference count, objects are then split
-- into further 8-byte slots representing the particular members of
-- the class (only fields, not methods); the identity of an object is
-- maintained at compile time, and at runtime objects are just opaque
-- data structures with no annotations as to which fields are which.
-- This means codegen needs to include a specific destructor function
-- for freeing each individual type of object. To make my life easier,
-- we also emit specific functions that are getters and setters for
-- specific properties of objects: they just read from and write to
-- the right offset for a given pointer to an object of the right
-- type.

-- Which brings us to name mangling. To make the linker happy, we'll
-- use Haskell-style name mangling with only alphanumeric characters
-- allowed, and we'll use `z` as our escape character: each `z` in a
-- function name gets translated to `zz`, which means `z.` where `.`
-- is any character is free for us to use for whatever purpose we
-- want. We define that dots in function names get translated to `zd`,
-- which means the function name `foo.Baz.xyz` gets translated to the
-- mangled name `foozdBazzzdxyzz`.

-- We'll use `zx` to represent internal functions. We define the
-- following:

--  The function `Foozxinit` will perform initialization of static
--  members of type `Foo`.

--  The function `Foozxalloc` will make an instance of `Foo`, which
--  means it'll allocate a block of the size of `Foo`, zero all of its
--  fields, set the reference count to 1, and return its address.

--  The function `Foozxgetzxbar` will take a pointer to an object
--  which is presumed to be of type `Foo`, and return the value `bar`
--  inside of it, whatever type that might happen to be; all types are
--  guaranteed to be at most 8 bytes long, so it just dumps it in
--  `%rax`, whether it's an integer, pointer, or float.

--  The function `Foozxsetzxbar` will take a pointer to an object of
--  type `Foo` and a new value of `bar`, and set the object's `bar`
--  property to the given value.

--  The function `Foozxdrop` will take a pointer to an object of type
--  `Foo`, and decrease any of the reference counts of any pointers it
--  owns by one, and potentially recursively call other `zxdrop`
--  functions.

--  Finally, to make dealing with primitive types easier, we'll define
--  functions `Foozxinc` and `Foozxdec` that do reference-count
--  increasing and reference-count decreasing on objects of type
--  `Foo`; for primitive types, these functions will do nothing.

-- Wow, that was longer than I thought it would be. Oh well, good
-- documentation on what you're doing never hurts.

module Codegen (codegen) where

import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.List (intercalate)
import Foreign
  ( Int64,
    Ptr,
    Storable (peek, poke),
    alloca,
    castPtr,
  )
import Parser
  ( ClassDecl (ClassDecl),
    ClassItem (ClassFnDecl, ClassVarDecl),
    ExecItem
      ( Block,
        ExecEval,
        ExecVarDecl,
        ForStmt,
        IfStmt,
        RetStmt,
        WhileStmt
      ),
    Expr
      ( AddOp,
        AndOp,
        Assignment,
        CallOp,
        Decrement,
        DivOp,
        DotOp,
        EqOp,
        GtOp,
        GteOp,
        Increment,
        LiteralExpr,
        LtOp,
        LteOp,
        ModOp,
        MulOp,
        Negate,
        NeqOp,
        NewArray,
        NewClass,
        NotOp,
        OrOp,
        SubOp,
        VarExpr
      ),
    FnDecl (FnDecl),
    Literal (CharLit, IntLit, RealLit, StringLit),
    SourceFile (SourceFile),
    VarDecl (VarDecl),
  )
import System.IO.Unsafe (unsafePerformIO)
import Typeck
  ( J0Type (ClassType, ObjectType),
    TypeEnv,
    desugarArithmetic,
    functionEnv,
    lookupType,
    toplevelTypeEnv,
    typeckExpr,
  )

-- Generates x86 assembly from a list of source files. Returns either
-- an error message, or the complete assembly code of the program.
codegen :: [SourceFile] -> Either String String
codegen files =
  let env = toplevelTypeEnv files
   in concat <$> sequence (codegenFile env <$> files)

codegenFile :: TypeEnv -> SourceFile -> Either String String
codegenFile env (SourceFile package imports classes) =
  concat <$> sequence (codegenClass env <$> classes)

codegenClass :: TypeEnv -> ClassDecl -> Either String String
codegenClass env cls =
  let (ClassDecl public static name members) = cls
      layout = do
        member <- members
        case member of
          ClassFnDecl _ -> []
          ClassVarDecl (VarDecl public static typ names) ->
            fst <$> names
   in concat
        <$> sequence
          [ codegenInitializer env cls,
            codegenAlloc env name layout,
            codegenGetSet env name layout,
            codegenFns env cls
          ]

-- Generates code to initialize static members of a class.
codegenInitializer :: TypeEnv -> ClassDecl -> Either String String
codegenInitializer env cls =
  -- TODO: implement this.
  pure "// initializer unimplemented\n"

-- Generates code to allocate a class.
codegenAlloc :: TypeEnv -> String -> [String] -> Either String String
codegenAlloc env className layout =
  let fnName = className ++ "zxalloc"
   in pure $
        unlines
          [ fnName ++ ":",
            "  movq $" ++ show (length layout * 8) ++ ", %rdi",
            "  call malloc",
            -- Zero out the memory in %eax..%eax + edi.
            "  movq %rax, %rbx",
            fnName ++ "_loop:",
            "  cmp $0, %rdi",
            "  jz " ++ fnName ++ "_fin",
            "  movq $0, (%rbx)",
            "  addq $8, %rbx",
            "  subq $8, %rdi",
            "  jmp " ++ fnName ++ "_loop",
            fnName ++ "_fin:",
            "  ret"
          ]

-- Generates getters and setters for each member of a class.
codegenGetSet :: TypeEnv -> String -> [String] -> Either String String
codegenGetSet env className layout =
  pure $ do
    (idx, fieldName) <- zip [0 ..] layout
    let offset = (idx + 1) * 8
    unlines
      [ className ++ "zxgetzx" ++ fieldName ++ ":",
        "  pushq %rbp",
        "  movq %rsp, %rbp",
        "  movq 16(%rbp), %rbx",
        "  movq %rax, " ++ show offset ++ "(%rbx)",
        "  popq %rbp",
        "  ret",
        className ++ "zxsetzx" ++ fieldName ++ ":",
        "  pushq %rbp",
        "  movq %rsp, %rbp",
        "  movq 16(%rbp), %rbx",
        "  movq 24(%rbp), %rcx",
        "  movq " ++ show offset ++ "(%rbx), %rcx",
        "  popq %rbp",
        "  ret"
      ]

-- Generates code for each function of a class.
codegenFns :: TypeEnv -> ClassDecl -> Either String String
codegenFns env (ClassDecl public static clsName members) =
  concat
    <$> sequence
      ( do
          member <- members
          case member of
            ClassVarDecl _ -> mempty
            ClassFnDecl fnDecl -> pure $ codegenFn env clsName fnDecl
      )

codegenFn :: TypeEnv -> String -> FnDecl -> Either String String
codegenFn env className (FnDecl public static retTyp fnName params body) =
  case body of
    Right body ->
      let mangledName = className ++ "zz" ++ fnName
          bodyEnv = functionEnv env body
          varLayout = layoutFunction body
          frameSize = length varLayout * 8
       in Right $
            unlines
              [ mangledName ++ ":",
                "  pushq %rbp",
                "  movq %rsp, %rbp",
                "  subq $" ++ show frameSize ++ ", %rsp"
              ]
              ++ concatMap (codegenStmt bodyEnv varLayout) body
    Left externName -> undefined

codegenStmt :: TypeEnv -> [String] -> ExecItem -> String
codegenStmt env layout item = case item of
  Block body -> undefined
  ExecVarDecl decl -> undefined
  ExecEval expr ->
    codegenExpr env layout expr
      ++ unlines
        [ "  addq $8, %rsp"
        ]
  RetStmt expr ->
    codegenExpr env layout expr
      ++ unlines
        [ "  popq %rax",
          "  mov %rbp, %rsp",
          "  popq %rbp",
          "  ret"
        ]
  IfStmt condition body elseBody -> undefined
  WhileStmt condition body -> undefined
  ForStmt init condition increment body -> undefined

codegenExpr :: TypeEnv -> [String] -> Expr -> String
codegenExpr env layout expr = case expr of
  Assignment l r -> undefined
  AddOp _ _ -> codegenExpr env layout $ desugarArithmetic expr
  SubOp _ _ -> codegenExpr env layout $ desugarArithmetic expr
  MulOp _ _ -> codegenExpr env layout $ desugarArithmetic expr
  DivOp _ _ -> codegenExpr env layout $ desugarArithmetic expr
  ModOp _ _ -> codegenExpr env layout $ desugarArithmetic expr
  Increment _ -> codegenExpr env layout $ desugarArithmetic expr
  Decrement _ -> codegenExpr env layout $ desugarArithmetic expr
  Negate _ -> codegenExpr env layout $ desugarArithmetic expr
  EqOp l r ->
    concat
      [ codegenExpr env layout l,
        codegenExpr env layout r,
        unlines
          [ "  popq %rax",
            "  popq %rbx",
            "  cmpq %rax,%rbx",
            "  je eq_success",
            "  pushq $0",
            "  jmp eq_end",
            "eq_success:",
            "  pushq $1",
            "eq_end:"
          ]
      ]
  NeqOp l r -> undefined
  GtOp l r -> undefined
  LtOp l r -> undefined
  GteOp l r -> undefined
  LteOp l r -> undefined
  AndOp l r -> undefined
  OrOp l r -> undefined
  NotOp e -> undefined
  CallOp e params -> do
    -- TODO: add error handling
    let DotOp eMain methodName = e
        Right (ObjectType eTyp) = typeckExpr env eMain
        Just (ClassType eClassName eClass) = lookupType env eTyp
        functionName = intercalate "zd" eTyp ++ methodName
    concat
      [ codegenExpr env layout eMain,
        concat (codegenExpr env layout <$> params),
        unlines
          [ "  call " ++ functionName,
            "  addq $" ++ show ((length params + 1) * 8) ++ ", %esp",
            "  pushq %eax"
          ]
      ]
  LiteralExpr lit -> case lit of
    IntLit n -> "  pushq $" ++ show n ++ "\n"
    RealLit n ->
      "  pushq $"
        ++ show
          ( unsafePerformIO $
              do
                alloca $ \ptr -> do
                  poke ptr n
                  peek (castPtr ptr :: Ptr Int64)
          )
        ++ "\n"
    CharLit c -> undefined
    StringLit s -> undefined
  VarExpr varname -> "_varexpr_\n"
  NewArray typ len -> undefined
  NewClass typ params -> undefined

-- Given a block, calculates a layout of variables within that block.
-- Returns the list of stored variables in order.
layoutFunction :: [ExecItem] -> [String]
layoutFunction items = do
  item <- items
  case item of
    ExecVarDecl (VarDecl public static typ names) -> do
      (name, initval) <- names
      pure name
    _ -> mempty
