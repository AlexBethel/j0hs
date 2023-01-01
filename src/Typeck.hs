module Typeck
  ( J0Type (..),
    TypeEnv (..),
    typeckExpr,
    lookupType,
    desugarArithmetic,
    toplevelTypeEnv,
    functionEnv,
  )
where

import Control.Monad (zipWithM_)
import Data.Bifunctor (first)
import Parser
  ( ClassDecl (..),
    ClassItem (..),
    ExecItem (..),
    Expr (..),
    FnDecl (..),
    Literal (..),
    SourceFile (..),
    TypeName (..),
    VarDecl (..),
  )

-- Types that an identifier or expression can have.
data J0Type
  = -- The type of a class that has the given members. Class types
    -- can't be stored in variables; they can only be used in
    -- expressions. The only supported operations on a class type are
    -- dot subscripting, which looks up members.
    ClassType
      { className :: String,
        classMembers :: [(String, J0Type)]
      }
  | -- The type of an object with the given name.
    ObjectType [String]
  | -- The type of a method that takes some list of parameter types,
    -- and that returns some type.
    MethodType
      { methodParams :: [J0Type],
        methodRetVal :: J0Type
      }
  | -- The type of an array of a particular type.
    ArrayType J0Type
  deriving (Eq, Show)

-- Environment of defined symbols.
newtype TypeEnv = TypeEnv [(String, J0Type)]
  deriving (Show)

lookupType :: TypeEnv -> [String] -> Maybe J0Type
lookupType (TypeEnv env) name = lookupType' env name
  where
    lookupType' :: [(String, J0Type)] -> [String] -> Maybe J0Type
    lookupType' names [] = error "empty type name"
    lookupType' names [name] = lookup name names
    lookupType' names (first : next) = do
      val <- lookup first names
      case val of
        ClassType name members -> lookupType' members next
        _ -> Nothing

nameToType :: TypeName -> J0Type
nameToType name = case name of
  BuiltinTypeName nm -> ObjectType [nm]
  ClassTypeName nm -> ObjectType nm
  ArrayTypeName base -> ArrayType $ nameToType base

-- Typechecks a particular expression, within the environment. Returns
-- either an error message, or the type of the expression.
typeckExpr :: TypeEnv -> Expr -> Either String J0Type
typeckExpr env expr = case expr of
  Assignment l r -> exprEq env l r
  AddOp _ _ -> typeckExpr env $ desugarArithmetic expr
  SubOp _ _ -> typeckExpr env $ desugarArithmetic expr
  MulOp _ _ -> typeckExpr env $ desugarArithmetic expr
  DivOp _ _ -> typeckExpr env $ desugarArithmetic expr
  ModOp _ _ -> typeckExpr env $ desugarArithmetic expr
  Increment _ -> typeckExpr env $ desugarArithmetic expr
  Decrement _ -> typeckExpr env $ desugarArithmetic expr
  Negate _ -> typeckExpr env $ desugarArithmetic expr
  EqOp l r -> do
    lt <- typeckExpr env l
    rt <- typeckExpr env r
    typEq lt rt
    pure $ ObjectType ["boolean"]
  NeqOp l r -> do
    lt <- typeckExpr env l
    rt <- typeckExpr env r
    typEq lt rt
    pure $ ObjectType ["boolean"]
  GtOp _ _ -> typeckExpr env $ desugarArithmetic expr
  LtOp _ _ -> typeckExpr env $ desugarArithmetic expr
  GteOp _ _ -> typeckExpr env $ desugarArithmetic expr
  LteOp _ _ -> typeckExpr env $ desugarArithmetic expr
  AndOp _ _ -> typeckExpr env $ desugarArithmetic expr
  OrOp _ _ -> typeckExpr env $ desugarArithmetic expr
  NotOp _ -> typeckExpr env $ desugarArithmetic expr
  InstanceOf l r -> do
    -- Wait, why does this operator exist in j0? In j0 there's no
    -- subclassing, therefore you can always tell this statically at
    -- compile time! ~~Alex
    lt <- typeckExpr env l
    rt <- typeckExpr env r
    case (lt, rt) of
      (ObjectType _, ClassType _ _) -> pure $ ObjectType ["boolean"]
      _ -> Left "illegal instanceof"
  SubscriptOp _ _ -> typeckExpr env $ desugarArithmetic expr
  DotOp e sub -> do
    et <- typeckExpr env e
    case et of
      ClassType name members -> case lookup sub members of
        Just mem -> pure mem
        Nothing -> Left ("no class member " ++ name ++ "." ++ sub)
      ObjectType objname ->
        let Just (ClassType className classMembers) = lookupType env objname
         in case lookup sub classMembers of
              Just member -> pure member
              Nothing ->
                Left
                  ( "missing member " ++ sub
                      ++ " of expression of type "
                      ++ show et
                      ++ " (members are " ++ show classMembers ++ ")"
                      ++ " (env is " ++ show env ++ ")"
                  )
      MethodType _ _ -> Left "illegal dot-subscript of method"
      ArrayType _ -> Left "array dot-subscript unimplemented"
  CallOp e args -> do
    et <- typeckExpr env e
    case et of
      MethodType params retval -> do
        if length params /= length args
          then
            Left
              ( "wrong number of arguments: expected "
                  ++ show (length params)
                  ++ ", got "
                  ++ show (length args)
              )
          else do
            argTypes <- sequence (typeckExpr env <$> args)
            zipWithM_ typEq params argTypes
            pure retval
  LiteralExpr literal -> case literal of
    IntLit _ -> pure (ObjectType ["int"])
    RealLit _ -> pure (ObjectType ["double"])
    CharLit _ -> pure (ObjectType ["char"])
    StringLit _ -> pure (ObjectType ["String"])
  VarExpr var -> case lookupType env [var] of
    Just typ -> pure typ
    Nothing -> Left ("undefined variable " ++ var)
  NewArray typename length -> undefined
  NewClass cls params -> do
    -- TODO: typecheck the parameters.
    case cls of
      ClassTypeName classname -> pure (ObjectType classname)
      _ -> undefined

-- Asserts that two types are equal.
typEq :: J0Type -> J0Type -> Either String J0Type
typEq expected got =
  if expected == got
    then Right expected
    else Left ("Type error: expected " ++ show expected ++ ", got " ++ show got)

-- Asserts that the types of two expressions are equal.
exprEq :: TypeEnv -> Expr -> Expr -> Either String J0Type
exprEq env l r = do
  lt <- typeckExpr env l
  rt <- typeckExpr env r
  typEq lt rt

-- Translates an expression into its underlying meaning. E.g.,
-- converts `a + b` into `a.__add__(b)`. Our dialect of J0 implements
-- operator overloading, unlike Java.
desugarArithmetic :: Expr -> Expr
desugarArithmetic e = case e of
  AddOp l r -> CallOp (DotOp l "__add__") [r]
  SubOp l r -> CallOp (DotOp l "__sub__") [r]
  MulOp l r -> CallOp (DotOp l "__mul__") [r]
  DivOp l r -> CallOp (DotOp l "__div__") [r]
  ModOp l r -> CallOp (DotOp l "__mod__") [r]
  Increment x -> Assignment x (AddOp x (LiteralExpr (IntLit 1)))
  Decrement x -> Assignment x (SubOp x (LiteralExpr (IntLit 1)))
  Negate x -> CallOp (DotOp x "__negate__") []
  GtOp l r -> CallOp (DotOp l "__gt__") [r]
  LtOp l r -> CallOp (DotOp l "__lt__") [r]
  GteOp l r -> CallOp (DotOp l "__gte__") [r]
  LteOp l r -> CallOp (DotOp l "__lte__") [r]
  AndOp l r -> CallOp (DotOp l "__and__") [r]
  OrOp l r -> CallOp (DotOp l "__or__") [r]
  NotOp x -> CallOp (DotOp x "__not__") []
  SubscriptOp l r -> CallOp (DotOp l "__subscript__") [r]

-- Builds a TypeEnv from a list of source files.
toplevelTypeEnv :: [SourceFile] -> TypeEnv
toplevelTypeEnv files =
  TypeEnv $ do
      SourceFile package imports classes <- files
      ClassDecl public static name members <- classes
      pure
        ( name,
          ClassType name $ do
            member <- members
            case member of
              ClassVarDecl
                ( VarDecl
                    _vPublic
                    vStatic
                    vType
                    vNames
                  ) -> do
                  (vName, _initVal) <- vNames
                  let typ = nameToType vType
                  pure (vName, typ)
              ClassFnDecl
                ( FnDecl
                    _fPublic
                    _fStatic
                    fRet
                    fName
                    fParams
                    _fBody
                  ) -> do
                  let retTyp = nameToType fRet
                      paramTypes = nameToType . fst <$> fParams
                  pure (fName, MethodType paramTypes retTyp)
        )

-- Builds a type environment for a function or block within a larger
-- type environment.
functionEnv :: TypeEnv -> [ExecItem] -> TypeEnv
functionEnv (TypeEnv parent) items =
  TypeEnv (childEnv items ++ parent)
  where
    childEnv :: [ExecItem] -> [(String, J0Type)]
    childEnv items = do
      item <- items
      case item of
        ExecVarDecl (VarDecl public static typ names) -> do
          (name, initVal) <- names
          [(name, nameToType typ)]
        _ -> []
