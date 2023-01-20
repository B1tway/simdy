{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecursiveDo #-}

module Emit where
import LLVM.Module
import LLVM.Context
import LLVM.Pretty (ppllvm)
import qualified Data.Text.Lazy.IO as TLIO

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
import LLVM.AST.Typed (getElementType, Typed (typeOf), getElementPtrType)
import LLVM.AST.Type (i32, float, void, i64)
import BuilderUtils
import LLVM.IRBuilder (int32, double)
import Syntax (PrimitiveType(I32))
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

type NameMap = Map.Map String AST.Operand
initNameMap :: NameMap
initNameMap = Map.empty

emit :: (LLVM.IRBuilder.Monad.MonadIRBuilder m, MonadModuleBuilder m, MonadState NameMap m) => Syn.Expr -> m AST.Operand
emit (Syn.Number i) = pure(int32 i)
emit (Syn.Decimal f) = pure(double f)
emit (Syn.Variable varname) =
    do
        varMap <- get
        let varOp = varMap Map.! varname
        load varOp
emit var@(Syn.DefVar varname vartype) =
    do
        newVar <- allocateDef var
        varMap <- get
        let newVarMap = Map.insert varname newVar varMap
        put newVarMap
        return newVar
emit (Syn.BinOp Syn.Assign a b) =
    do
        varOperand <- emit a
        value <- emit b
        store varOperand value
        return value
emit (Syn.BinOp op a b) =
    do
        opA <- emit a
        opB <- emit b
        let aType = getElemType ( typeOf opA)
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
emit (Syn.MemOp (Syn.Store stype sptr svalue)) =
    do
        ptr <- emit sptr
        value <- emit svalue
        store ptr value
        return value 
emit (Syn.MemOp (Syn.Load stype sptr svalue)) =
    do
        ptr <- emit sptr
        value <- emit svalue
        sextValue <- sext value i64
        temp <- allocate (toLLVMType stype)
        newAddr <- gep ptr ([sextValue])
        load newAddr
emit expr = error ("Impossible expression <" ++ show expr ++ ">")

buildCodeBlock :: (MonadIRBuilder m, MonadModuleBuilder m, MonadState NameMap m) => [Syn.Expr] -> m AST.Operand
buildCodeBlock exprBlock = do
  -- Steps of codegen
  ops <- mapM emit exprBlock
  return (last ops)

-- funcBodyBuilder :: (MonadFix m, MonadIRBuilder m) => [Syn.Expr] -> [Syn.Expr] -> ([AST.Operand] -> m ())
funcBodyBuilder :: (MonadIRBuilder m, MonadModuleBuilder m, MonadState NameMap m) =>[Syn.Expr] -> [Syn.Expr] -> ([AST.Operand] -> m ())
funcBodyBuilder bodyTokens args = funcBody
    where funcBody argsOperands = do
            named block bodyLabel
            allocArgs args
            result <- buildCodeBlock bodyTokens
            retVoid

allocArgs :: (MonadIRBuilder m, MonadState NameMap m) => [Syn.Expr] -> m ()
allocArgs (e@(Syn.DefVar varname vartype) : exprs) = do
  p <- allocateT vartype `named` toShort' varname
  store p (referenceLocal vartype $ argName varname)
  varMap <- get
  let newVarMap = Map.insert varname p varMap
  put newVarMap
  allocArgs exprs
allocArgs [] = pure ()


-- buildFunction :: MonadModuleBuilder m => Syn.Expr -> m AST.Operand
buildFunction :: (MonadModuleBuilder m,
 MonadState (Map.Map String AST.Operand) m) => Syn.Expr -> m AST.Operand
buildFunction func@(Syn.Function name argsNames body) =
  function(AST.Name fname) fargs VoidType funcBody
  where
    fname = toShort' name
    fargs = map argDef argsNames
    funcBody = funcBodyBuilder body argsNames


-- parseTopLevel :: (MonadModuleBuilder m, MonadFix m) => [Syn.Expr] -> m ()
parseTopLevel :: (MonadModuleBuilder m, MonadState (Map.Map String AST.Operand) m) => [Syn.Expr] -> m ()
parseTopLevel (e:es) = do
  buildFunction e >> pure ()
  parseTopLevel es
parseTopLevel [] = pure ()

buildIR :: [Syn.Expr] -> AST.Module
buildIR exprs = evalState (buildModuleT "program" $ parseTopLevel exprs) initNameMap

printIR :: AST.Module -> IO ()
printIR ir = TLIO.putStrLn $ ppllvm ir