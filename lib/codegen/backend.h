#ifndef BACKEND
#define BACKEND

#include "ast.pb-c.h"
#include <llvm-c/Analysis.h>
#include <llvm-c/Core.h>
#include <llvm-c/Types.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

void gen_block (LLVMContextRef context, LLVMModuleRef module,
                LLVMBuilderRef builder, SBlock *block, bool global);

LLVMValueRef gen_expr (LLVMContextRef context, LLVMModuleRef module,
                       LLVMBuilderRef builder, Expression *expr);

void gen_stmt (LLVMContextRef context, LLVMModuleRef module,
               LLVMBuilderRef builder, Statement *stmt, bool global);

#endif
