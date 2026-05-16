#include "backend.h"

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Core.h>
#include <llvm-c/IRReader.h>
#include <llvm-c/TargetMachine.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.pb-c.h"

CAMLprim value
codegen (value astModule, value astModuleLen)
{
  CAMLparam2 (astModule, astModuleLen);

  const uint8_t *astB = (const uint8_t *)String_val (astModule);
  size_t astS = Int_val (astModuleLen);

  Module *mod = module__unpack (NULL, astS, astB);
  LLVMContextRef context = LLVMContextCreate ();

  LLVMModuleRef module
      = LLVMModuleCreateWithNameInContext (mod->name, context);

  if (strcmp (mod->name, "main") == 0)
    {
      LLVMTypeRef paramTypes[] = {};
      LLVMTypeRef returnType = LLVMFunctionType (
          LLVMInt32TypeInContext (context), paramTypes, 0, 0);
      LLVMValueRef fnMain = LLVMAddFunction (module, "main", returnType);

      LLVMBuilderRef fnB = LLVMCreateBuilderInContext (context);
      LLVMBasicBlockRef blockEntry
          = LLVMAppendBasicBlockInContext (context, fnMain, "entry");
      LLVMPositionBuilderAtEnd (fnB, blockEntry);
      LLVMBuildRet (fnB,
                    LLVMConstInt (LLVMInt32TypeInContext (context), 0, 0));
    }

  char *error = NULL;
  LLVMVerifyModule (module, LLVMAbortProcessAction, &error);

  char *output = LLVMPrintModuleToString (module);
  CAMLreturn (caml_copy_string (output));
}

CAMLprim value
compile (value module, value n_module)
{
  CAMLparam2 (module, n_module);
  LLVMMemoryBufferRef buf = LLVMCreateMemoryBufferWithMemoryRange (
      String_val (module), Int_val (n_module), "ir", 0);
  LLVMContextRef context = LLVMContextCreate ();
  LLVMModuleRef mod;
  char *error;

  if (LLVMParseIRInContext (context, buf, &mod, &error))
    {
      CAMLreturn (Int_val (1));
    }

  LLVMInitializeX86TargetInfo ();
  LLVMInitializeX86Target ();
  LLVMInitializeX86TargetMC ();
  LLVMInitializeX86AsmPrinter ();

  char *targetTriple = LLVMGetDefaultTargetTriple ();
  LLVMTargetRef target;
  LLVMGetTargetFromTriple (targetTriple, &target, &error);

  LLVMTargetMachineRef machine = LLVMCreateTargetMachine (
      target, targetTriple, "generic", "", LLVMCodeGenLevelDefault,
      LLVMRelocDefault, LLVMCodeModelDefault);

  size_t filename_len;
  const char *filename = LLVMGetSourceFileName (mod, &filename_len);

  char filename_o[filename_len * 2];
  sprintf (filename_o, "%s.o", filename);
  LLVMTargetMachineEmitToFile (machine, mod, filename_o, LLVMObjectFile,
                               &error);
  CAMLreturn (caml_copy_string (filename_o));
}
