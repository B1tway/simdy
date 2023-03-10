{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fno-warn-incomplete-uni-patterns#-}

module ASTBridge (findOperation, toLLVMType, getElemType) where

import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

import qualified LLVM.AST.FloatingPointPredicate as FPredicats
import qualified LLVM.AST.IntegerPredicate as IPredicats
import LLVM.AST.Operand
import LLVM.AST.Type
import LLVM.IRBuilder.Constant ()
import LLVM.IRBuilder.Instruction hiding (load, store)
import LLVM.IRBuilder.Monad

import Syntax as Syn

getElemType :: LLVM.AST.Type.Type -> LLVM.AST.Type.Type
getElemType (LLVM.AST.Type.IntegerType _) = i32
getElemType (LLVM.AST.Type.FloatingPointType _) = float
getElemType (LLVM.AST.Type.VectorType _ t) = getElemType t
getElemType t = t

typeMap :: Map.Map PrimitiveType LLVM.AST.Type.Type
typeMap =
    Map.fromList
        [ (I32, i32)
        , (U32, i32)
        , (I16, i16)
        , (U16, i16)
        , (FLOAT, float)
        , (DOUBLE, double)
        ]

exprToInt :: Expr -> Word32
exprToInt (Number i) = fromIntegral i
exprToInt _ = 0

toLLVMType :: Syn.Type -> LLVM.AST.Type.Type
toLLVMType (Primitive t) = typeMap ! t
toLLVMType (Ptr t) = ptr (toLLVMType t)
toLLVMType (VectType e t) =
    VectorType{nVectorElements = exprToInt e, elementType = typeMap ! t}

opTable ::
    MonadIRBuilder m =>
    Map.Map
        LLVM.AST.Type.Type
        (Map.Map Op (Operand -> Operand -> m Operand))
opTable =
    Map.fromList
        [(i16, intMap), (i32, intMap), (float, floatMap), (double, floatMap)]
    where
        [intMap, floatMap] =
            map
                Map.fromList
                [
                    [ (Plus, add)
                    , (Minus, sub)
                    , (Times, mul)
                    , (Less, icmp IPredicats.SLT)
                    , (Greater, icmp IPredicats.SGT)
                    , (Equal, icmp IPredicats.EQ)
                    , (NotEqual, icmp IPredicats.NE)
                    ]
                ,
                    [ (Plus, fadd)
                    , (Minus, fsub)
                    , (Times, fmul)
                    , (Less, fcmp FPredicats.OLT)
                    , (Greater, fcmp FPredicats.OGT)
                    , (Equal, fcmp FPredicats.OEQ)
                    , (NotEqual, fcmp FPredicats.ONE)
                    ]
                ]

findOperation ::
    MonadIRBuilder m =>
    LLVM.AST.Type.Type ->
    Op ->
    Operand ->
    Operand ->
    m Operand
findOperation type_ op = (opTable ! type_) ! op
