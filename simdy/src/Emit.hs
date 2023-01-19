{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


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


emit :: LLVM.IRBuilder.Monad.MonadIRBuilder m => Syn.Expr -> m AST.Operand
emit (Syn.Number i) = pure(int32 i)
emit (Syn.Decimal f) = pure(double f)
emit (Syn.Variable varname) = load (referenceVar (Syn.Primitive  Syn.I32) varname) 
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



-- buildFunction :: (MonadModuleBuilder m, MonadFix m) => Expr -> m Operand
-- buildFunction (Syn.Function fname fargs fbody) func@(TFunction name argsNames body) =
--   function (Name $ toShort' name) arguments (toLLVMType retType) funcBody
--   where typedArgs =  [TypedExpr (argsTypes !! i) (TDef (argsNames !! i)) | i <- [0..(length argsNames - 1)]]
--         arguments = map argDef typedArgs
--         funcBody = funcBodyBuilder body typedArgs