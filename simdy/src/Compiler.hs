module Compiler (writeObject) where

import qualified LLVM.AST
import LLVM.Internal.Context (Context, withContext)
import LLVM.Internal.Target (withHostTargetMachineDefault)
import LLVM.Module (File, Module, withModuleFromAST, writeObjectToFile)

writeWithDefaultTarget :: File -> Module -> IO ()
writeWithDefaultTarget file inputModule
  = withHostTargetMachineDefault (\ t -> writeObjectToFile t file inputModule)

writeWithModuleFromAST :: File -> Context -> LLVM.AST.Module -> IO ()
writeWithModuleFromAST f c m = withModuleFromAST c m (writeWithDefaultTarget f)

writeObject :: File -> LLVM.AST.Module -> IO ()
writeObject file module'
  = withContext (\ c -> writeWithModuleFromAST file c module')
