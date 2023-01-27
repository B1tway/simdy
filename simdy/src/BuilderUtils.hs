{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-incomplete-patterns#-}
{-# OPTIONS -fno-warn-missing-export-lists#-}

module BuilderUtils where
import ASTBridge (toLLVMType)
import Data.ByteString.Short (ShortByteString)

import Data.Word (Word32)

import LLVM.AST hiding (Call, alignment, function)
import qualified LLVM.AST as A
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Constant as C
import LLVM.AST.Type as AST
import LLVM.IRBuilder.Constant hiding (double)
import LLVM.IRBuilder.Instruction hiding (load, store)
import qualified LLVM.IRBuilder.Instruction as I
import LLVM.IRBuilder.Module (ParameterName(ParameterName))

import LLVM.IRBuilder.Monad
import StringUtils (toShort')
import Syntax (Expr)
import qualified Syntax as Syn

addrSpace :: AddrSpace
addrSpace = AddrSpace 0

iSize :: Word32
iSize = 32

alignment :: Word32
alignment = 4

referenceLocal :: Syn.Type -> String -> Operand
referenceLocal varType = reference (toLLVMType varType)

argDef :: Expr -> (Type, ParameterName)
argDef (Syn.DefVar defName defType)
  = (toLLVMType defType, ParameterName $ toShort' (argName defName))

allocateDef :: MonadIRBuilder m => Syn.Expr -> m Operand
allocateDef (Syn.DefVar varname vartype)
  = named (allocateT vartype) (toShort' varname)

allocateT :: MonadIRBuilder m => Syn.Type -> m Operand
allocateT t = allocate (toLLVMType t)

integerConstant :: Integer -> Operand
integerConstant i
  = ConstantOperand (C.Int{C.integerBits = iSize, C.integerValue = i})

integerPointer :: AST.Type
integerPointer = AST.PointerType i32 addrSpace

allocate :: MonadIRBuilder m => AST.Type -> m Operand
allocate type_ = alloca type_ Nothing alignment

allocateInt :: MonadIRBuilder m => m Operand
allocateInt = allocate i32

load :: MonadIRBuilder m => Operand -> m Operand
load pointer = I.load pointer alignment

store :: MonadIRBuilder m => Operand -> Operand -> m ()
store pointer = I.store pointer alignment

saveInt :: MonadIRBuilder m => Integer -> m Operand
saveInt ivalue
  = do pointer <- allocateInt
       store pointer (int32 ivalue)
       return pointer

refName :: String -> A.Name
refName name = Name (toShort' $ name ++ "_0")

globalName :: String -> A.Name
globalName name = Name (toShort' name)

reference :: AST.Type -> String -> Operand
reference type_ name = LocalReference type_ (refName name)

pointerTo :: Syn.Type -> Type
pointerTo type_ = AST.PointerType (toLLVMType type_) addrSpace

referenceVar :: Syn.Type -> String -> Operand
referenceVar varType = reference (pointerTo varType)

referenceInt :: String -> Operand
referenceInt = reference i32

referenceIntPointer :: String -> Operand
referenceIntPointer = reference integerPointer

makeFuncRef :: String -> Operand
makeFuncRef funcName
  = ConstantOperand (C.GlobalReference funcType $ globalName funcName)
  where funcType = FunctionType void [] False

bodyLabel :: ShortByteString
bodyLabel = toShort' "Body"

argName :: String -> String
argName = ("arg_" ++)

intToFloat :: MonadIRBuilder m => Operand -> m Operand
intToFloat op = sitofp op double

floatToInt :: MonadIRBuilder m => Operand -> m Operand
floatToInt op = fptosi op i32
