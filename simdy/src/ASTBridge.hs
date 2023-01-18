module ASTBridge where

import           Data.Map.Strict                 ((!))
import qualified Data.Map.Strict                 as Map
import Data.Word (Word32)
import           LLVM.AST.Operand
import           LLVM.AST.Type
import           LLVM.IRBuilder.Constant         hiding (double)
import           LLVM.IRBuilder.Instruction      hiding (load, store)
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad

import qualified LLVM.AST.FloatingPointPredicate as FPredicats
import qualified LLVM.AST.IntegerPredicate       as IPredicats

import           Syntax as Syn
import Data.ByteString.Short (toShort)
import LLVM.AST.Operand (DIGlobalVariable(type'))


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
toLLVMType (VectType e t) = VectorType {nVectorElements = exprToInt e, elementType = typeMap ! t}

-- allocateDef (Expr (DefVar name' type')) = allocate (toLLVMType type') 'named' toShort name


cmpOps :: [String]
cmpOps = [">", "<", "==", "!=", "<=", ">="]

opTable ::
     MonadIRBuilder m
  => Map.Map PrimitiveType (Map.Map String (Operand -> Operand -> m Operand))
opTable =
  Map.fromList
    [ (I32, intMap)
    , (U32, intMap)
    , (I16, intMap)
    , (U16, intMap)
    , (FLOAT, floatMap)
    , (DOUBLE, floatMap)
    ]
  where
    [intMap, floatMap] =
      map
        Map.fromList
        [ [ ("+", add)
          , ("-", sub)
          , ("*", mul)
          , ("<", icmp IPredicats.SLT)
          , (">", icmp IPredicats.SGT)
          , ("==", icmp IPredicats.EQ)
          , ("!=", icmp IPredicats.NE)
          , ("<=", icmp IPredicats.SLE)
          , (">=", icmp IPredicats.SGE)
          ]
        , [ ("+", fadd)
          , ("-", fsub)
          , ("*", fmul)
          , ("<", fcmp FPredicats.OLT)
          , (">", fcmp FPredicats.OGT)
          , ("==", fcmp FPredicats.OEQ)
          , ("!=", fcmp FPredicats.ONE)
          , ("<=", fcmp FPredicats.OLE)
          , (">=", fcmp FPredicats.OGE)
          ]
        ]


findOperation type_ op = (opTable ! type_) ! op
