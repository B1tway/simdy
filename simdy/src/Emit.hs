{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecursiveDo #-}

module Emit where
import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction hiding (load, store)
import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map

import Codegen

import qualified Syntax as Syn
import LLVM.AST (Type(VoidType))
import ASTBridge

import StringUtils
import LLVM.AST.Typed (getElementType, Typed (typeOf))
import LLVM.AST.Type (i32, float)
import BuilderUtils
import LLVM.IRBuilder (int32, double)
import Syntax (PrimitiveType(I32))
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

type NameMap = Map.Map String AST.Operand
initNameMap :: NameMap
initNameMap = Map.empty

emit :: (LLVM.IRBuilder.Monad.MonadIRBuilder m, MonadState NameMap m) => Syn.Expr -> m AST.Operand
emit (Syn.Number i) = pure(int32 i)
emit (Syn.Decimal f) = pure(double f)
emit (Syn.Variable varname) =
    do
        varMap <- get
        let varOp = varMap Map.! varname
        load varOp
emit var@(Syn.DefVar varname vartype) = allocateDef var
emit (Syn.BinOp Syn.Assign (Syn.Variable varname) b) =
    do
        value <- emit b
        store (referenceVar (Syn.Primitive  Syn.I32) varname) value
        return value
emit (Syn.BinOp op a b) =
    do
        opA <- emit a
        opB <- emit b
        let aType = getElementType (typeOf opA)
        findOperation aType op opA opB
emit (Syn.Call fname fargs) =
    do
        args <- emitArgs fargs
        call (makeFuncRef fname) args
    where
        emitArgs (e:es) = do
            arg <- emit e
            args <- emitArgs es
            return ((arg, []) : args)
        emitArgs _ = return []


allocArgs :: MonadIRBuilder m => [Syn.Expr] -> m ()
allocArgs (e@(Syn.DefVar varname vartype) : exprs) = do
  p <- allocateT vartype `named` toShort' varname
  store p (referenceLocal vartype $ argName varname)
  allocArgs exprs
allocArgs [] = pure ()


-- buildFunction :: (MonadModuleBuilder m, MonadFix m) => Syn.Expr -> m AST.Operand
-- buildFunction (Syn.Function fname fargs fbody) =
--     where
--         args = map argDef 

-- parseTopLevel :: (MonadModuleBuilder m, MonadFix m) => [Syn.Expr] -> m ()
-- parseTopLevel (e:es) = do
--   buildFunction e >> pure ()
--   parseTopLevel es
-- parseTopLevel [] = pure ()

-- buildIR :: [Syn.Expr] -> AST.Module
-- buildIR exprs = buildModule "program" $ parseTopLevel exprs