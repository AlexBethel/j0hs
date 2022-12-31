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

import Control.Monad (forM_)
import Control.Monad.State (StateT (runStateT), get, gets, put)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
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
    VarDecl (VarDecl, varNames, varStatic),
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

-- Monad used for code generation. Can fail with an error message, and
-- change the state of the function-local information, and emit
-- arbitrary lines of assembly code, while maintaining some value.
type Codegen a = ExceptT String (StateT CodegenState (Writer [String])) a

-- State of the code generator.
data CodegenState = CodegenState
  { stateEnv :: TypeEnv,
    nextLabelIndex :: Int
  }

-- Generates x86 assembly from a list of source files. Returns either
-- an error message, or the complete assembly code of the program.
codegen :: [SourceFile] -> Either String String
codegen files =
  let exceptStep = runExceptT (codegenInner files)
      stateStep =
        runStateT exceptStep $
          CodegenState
            { stateEnv = toplevelTypeEnv files,
              nextLabelIndex = 0
            }
      writerStep = runWriter stateStep
      ((maybeError, finalState), code) = writerStep
   in do
        maybeError
        pure $ unlines code

putInstruction :: String -> Codegen ()
putInstruction = lift . lift . tell . pure

putInstructions :: [String] -> Codegen ()
putInstructions = mapM_ putInstruction

label :: Codegen String
label = do
  state <- get
  let idx = nextLabelIndex state
  put $
    CodegenState
      { stateEnv = stateEnv state,
        nextLabelIndex = idx + 1
      }
  pure $ "zxlabel" ++ show idx

codegenInner :: [SourceFile] -> Codegen ()
codegenInner = mapM_ codegenFile

codegenFile :: SourceFile -> Codegen ()
codegenFile (SourceFile package imports classes) =
  mapM_ codegenClass classes

codegenClass :: ClassDecl -> Codegen ()
codegenClass cls = do
  let ClassDecl public static name members = cls
      layout = genLayout members
  codegenInitializer cls
  codegenAlloc name layout
  codegenGetSet name layout
  mapM_ (codegenFn name) members

type VarLayout = [String]

genLayout :: [ClassItem] -> VarLayout
genLayout members = do
  member <- members
  case member of
    ClassVarDecl (VarDecl {varStatic = False, varNames = names}) ->
      fst <$> names
    _ -> mempty

-- Generates code to initialize static members of a class.
codegenInitializer :: ClassDecl -> Codegen ()
codegenInitializer _ = putInstruction "// initializer unimplemented"

-- Generates code to allocate an instance of a class.
codegenAlloc :: String -> VarLayout -> Codegen ()
codegenAlloc clsName layout = do
  let fnName = clsName ++ "zxalloc"
  putInstructions
    [ fnName ++ ":",
      "  movq $" ++ show (length layout * 8) ++ ", %rdi",
      "  call malloc",
      "  addq $8, %rsp",
      -- Zero out the memory in %rax..%rax + rdi.
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
codegenGetSet :: String -> VarLayout -> Codegen ()
codegenGetSet clsName layout =
  forM_ (zip [0 ..] layout) $ \(idx, fieldName) -> do
    let offset = idx * 8
    putInstructions
      [ clsName ++ "zxgetzx" ++ fieldName ++ ":",
        "  pushq %rbp",
        "  movq %rsp, %rbp",
        "  movq 16(%rbp), %rbx",
        "  mov " ++ show offset ++ "(%rbx), %rax",
        "  popq %rbp",
        "  ret"
      ]
    putInstructions
      [ clsName ++ "zxsetzx" ++ fieldName ++ ":",
        "  pushq %rbp",
        "  movq %rsp, rbp",
        "  movq 16(%rbp), %rbx",
        "  movq 24(%rbp), %rcx",
        "  movq %rcx, " ++ show offset ++ "(%rbx)",
        "  popq %rbp",
        "  ret"
      ]

-- Generates code for a function of a class.
codegenFn :: String -> ClassItem -> Codegen ()
codegenFn clsName (ClassVarDecl vd) = pure ()
codegenFn clsName (ClassFnDecl (FnDecl public static retTyp fnName params body)) =
  case body of
    Right body -> do
      env <- gets stateEnv
      let mangledName = clsName ++ "zd" ++ fnName
          bodyEnv = functionEnv env body
          layout = layoutFunction body
          frameSize = length layout * 8
      putInstructions
        [ mangledName ++ ":",
          "  pushq %rbp",
          "  movq %rsp, %rbp",
          "  subq $" ++ show frameSize ++ ", %rsp"
        ]
      mapM_ (codegenStmt bodyEnv layout) body

codegenStmt :: TypeEnv -> VarLayout -> ExecItem -> Codegen ()
codegenStmt env layout stmt = case stmt of
  Block body ->
    -- BUG: this doesn't deal with new variable allocs right
    mapM_ (codegenStmt env layout) body
  ExecVarDecl decl -> pure () -- !!
  ExecEval expr -> do
    codegenExpr env layout expr
    putInstruction "  addq $8, %rsp"
  RetStmt expr -> do
    codegenExpr env layout expr
    putInstructions
      [ "  popq %rax",
        "  movq %rbp, %rsp",
        "  popq %rbp",
        "  ret"
      ]
  IfStmt condition body elseBody -> do
    endLabel <- label
    elseLabel <- label

    codegenExpr env layout condition
    putInstructions
      [ "  popq %rax",
        "  cmpq $0, %rax",
        "  je " ++ elseLabel
      ]
    codegenStmt env layout body
    putInstruction $ "  jmp " ++ endLabel

    putInstruction $ elseLabel ++ ":"
    codegenStmt env layout (fromMaybe (Block []) elseBody)
    putInstruction $ endLabel ++ ":"
  WhileStmt condition body -> do
    startLabel <- label
    endLabel <- label

    putInstruction $ startLabel ++ ":"
    codegenExpr env layout condition
    putInstructions
      [ "  popq %rax",
        "  cmpq $0, %rax",
        "  je " ++ endLabel
      ]
    codegenStmt env layout body
    putInstructions
      [ "  jmp " ++ startLabel,
        endLabel ++ ":"
      ]
  ForStmt init condition increment body ->
    -- fuck it, let's convert this into a while loop.
    codegenStmt env layout $
      Block
        [ ExecEval init,
          WhileStmt condition $
            Block
              [ body,
                ExecEval increment
              ]
        ]

codegenExpr :: TypeEnv -> VarLayout -> Expr -> Codegen ()
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
  EqOp l r -> do
    codegenExpr env layout l
    codegenExpr env layout r

    successLabel <- label
    endLabel <- label
    putInstructions
      [ "  popq %rax",
        "  popq %rbx",
        "  cmpq %rax,%rbx",
        "  je " ++ successLabel,
        "  pushq $0",
        "  jmp " ++ endLabel,
        successLabel ++ ":",
        "  pushq $1",
        endLabel ++ ":"
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
    (eMain, methodName) <- case e of
      DotOp l r -> pure (l, r)
      _ -> throwE "function call on something not a method"
    eTyp <- case typeckExpr env eMain of
      Right (ObjectType t) -> pure t
      Right _ -> throwE "static calls unimplemented"
      Left typeError -> throwE typeError

    -- This can't fail because typeckExpr always returns a valid type.
    let Just (ClassType eClassName eClass) = lookupType env eTyp
        functionName = intercalate "zd" (eTyp ++ [methodName])

    mapM_ (codegenExpr env layout) (reverse params)
    codegenExpr env layout eMain
    putInstructions
      [ "  call " ++ functionName,
        "  addq $" ++ show ((length params + 1) * 8) ++ ", %rsp",
        "  pushq %rax"
      ]
  LiteralExpr lit -> case lit of
    IntLit n -> putInstruction $ "  pushq $" ++ show n
    RealLit n ->
      putInstruction $
        "  pushq $"
          ++ show
            ( unsafePerformIO $
                do
                  alloca $ \ptr -> do
                    poke ptr n
                    peek (castPtr ptr :: Ptr Int64)
            )
    CharLit c -> undefined
    StringLit s -> undefined
  VarExpr varname -> do
    varOffset <- case lookup varname (zip layout [1 ..]) of
      Just offset -> pure offset
      Nothing -> throwE $ "undefined variable " ++ varname
    putInstructions
      [ "  mov " ++ show (-varOffset * 8) ++ "(%rbp), %rax",
        "  pushq %rax"
      ]
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
