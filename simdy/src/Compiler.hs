module Compiler(writeObject) where

import qualified LLVM.AST
import LLVM.Internal.Target (withHostTargetMachineDefault)
import LLVM.Internal.Context (withContext, Context)
import LLVM.Module (withModuleFromAST, writeObjectToFile, Module, File)

writeWithDefaultTarget :: File -> Module -> IO ()
writeWithDefaultTarget file inputModule= withHostTargetMachineDefault (\t -> writeObjectToFile t file inputModule)

writeWithModuleFromAST :: File -> Context -> LLVM.AST.Module -> IO ()
writeWithModuleFromAST f c m = withModuleFromAST c m (writeWithDefaultTarget f)

writeObject :: File -> LLVM.AST.Module -> IO ()
writeObject file module' = withContext (\c -> writeWithModuleFromAST file c module')