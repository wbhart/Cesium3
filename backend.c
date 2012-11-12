/*

Copyright 2012 William Hart. All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are
permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, this list of
      conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright notice, this list
      of conditions and the following disclaimer in the documentation and/or other materials
      provided with the distribution.

THIS SOFTWARE IS PROVIDED BY William Hart ``AS IS'' AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL William Hart OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

#include "backend.h"

/* 
   Tell LLVM about some external library functions so we can call them 
   and about some constants we want to use from jit'd code
*/
void llvm_functions(jit_t * jit)
{
   LLVMTypeRef args[2];
   LLVMTypeRef fntype; 
   LLVMTypeRef ret;
   LLVMValueRef fn;

   /* patch in the printf function */
   args[0] = LLVMPointerType(LLVMInt8Type(), 0);
   ret = LLVMWordType();
   fntype = LLVMFunctionType(ret, args, 1, 1);
   fn = LLVMAddFunction(jit->module, "printf", fntype);
}

/* count parameters as represented by %'s in format string */
int count_params(const char * fmt)
{
   int len = strlen(fmt);
   int i, count = 0;

   for (i = 0; i < len - 1; i++)
      if (fmt[i] == '%')
         if (fmt[i + 1] == '%')
               i++;
         else
            count++;

   return count;
}

/* 
   When an expression is evaluated it is a pain to pass it back out of
   jit'd code and print the value returned by the expression. So we 
   print it on the jit side. So this is our runtime printf for 
   printing LLVMValRef's from jit'd code.
*/
void llvm_printf(jit_t * jit, const char * fmt, ...)
{
   int i, count = count_params(fmt);
   va_list ap;
   
   /* get printf function */
   LLVMValueRef fn = LLVMGetNamedFunction(jit->module, "printf");
   LLVMSetFunctionCallConv(fn, LLVMCCallConv);
   
   /* Add a global variable for format string */
   LLVMValueRef str = LLVMConstString(fmt, strlen(fmt), 0);
   jit->fmt_str = LLVMAddGlobal(jit->module, LLVMTypeOf(str), "fmt");
   LLVMSetInitializer(jit->fmt_str, str);
   LLVMSetGlobalConstant(jit->fmt_str, 1);
   LLVMSetLinkage(jit->fmt_str, LLVMInternalLinkage);
   
   /* build variadic parameter list for printf */
   LLVMValueRef indices[2] = { LLVMConstInt(LLVMWordType(), 0, 0), LLVMConstInt(LLVMWordType(), 0, 0) };
   LLVMValueRef GEP = LLVMBuildGEP(jit->builder, jit->fmt_str, indices, 2, "str");
   LLVMValueRef args[count + 1];
   args[0] = GEP;
   
   va_start(ap, fmt);

   for (i = 0; i < count; i++)
      args[i + 1] = va_arg(ap, LLVMValueRef);

   va_end(ap);

   /* build call to printf */
   LLVMValueRef call_printf = LLVMBuildCall(jit->builder, fn, args, count + 1, "printf");
   LLVMSetTailCall(call_printf, 1);
   LLVMAddInstrAttribute(call_printf, 0, LLVMNoUnwindAttribute);
   LLVMAddInstrAttribute(call_printf, 1, LLVMNoAliasAttribute);
}

/*
   This jits a printf for various cesium types. We use it to print
   the result of expressions that are evaluated, before returning from
   a jit'd expression.
*/
void print_obj(jit_t * jit, type_t * type, LLVMValueRef obj)
{
   switch (type->typ)
   {
      case INT:
         llvm_printf(jit, "%ld", obj);
         break;
      case CHAR:
         llvm_printf(jit, "%c", obj);
         break;
      case DOUBLE:
         llvm_printf(jit, "%.5g", obj);
         break;
      case STRING:
         llvm_printf(jit, "\"%s\"", obj);
         break;
   }
}

/*
   Initialise the LLVM JIT
*/
jit_t * llvm_init(void)
{
    char * error = NULL;
    
    /* create jit struct */
    jit_t * jit = (jit_t *) GC_MALLOC(sizeof(jit_t));

    /* Jit setup */
    LLVMLinkInJIT();
    LLVMInitializeNativeTarget();

    /* Create module */
    jit->module = LLVMModuleCreateWithName("cesium");

    /* Create JIT engine */
    if (LLVMCreateJITCompilerForModule(&(jit->engine), jit->module, 2, &error) != 0) 
    {   
       fprintf(stderr, "%s\n", error);  
       LLVMDisposeMessage(error);  
       abort();  
    } 
   
    /* Create optimisation pass pipeline */
    jit->pass = LLVMCreateFunctionPassManagerForModule(jit->module);  
    LLVMAddTargetData(LLVMGetExecutionEngineTargetData(jit->engine), jit->pass);  
    LLVMAddAggressiveDCEPass(jit->pass); /* */
    LLVMAddDeadStoreEliminationPass(jit->pass); 
    LLVMAddIndVarSimplifyPass(jit->pass); 
    LLVMAddJumpThreadingPass(jit->pass); 
    LLVMAddLICMPass(jit->pass); 
    LLVMAddLoopDeletionPass(jit->pass); 
    LLVMAddLoopRotatePass(jit->pass); 
    LLVMAddLoopUnrollPass(jit->pass); 
    LLVMAddLoopUnswitchPass(jit->pass);
    LLVMAddMemCpyOptPass(jit->pass); 
    LLVMAddReassociatePass(jit->pass); 
    LLVMAddSCCPPass(jit->pass); 
    LLVMAddScalarReplAggregatesPass(jit->pass); 
    LLVMAddSimplifyLibCallsPass(jit->pass);
    LLVMAddTailCallEliminationPass(jit->pass); 
    LLVMAddDemoteMemoryToRegisterPass(jit->pass); /* */ 
    LLVMAddConstantPropagationPass(jit->pass);  
    LLVMAddInstructionCombiningPass(jit->pass);  
    LLVMAddPromoteMemoryToRegisterPass(jit->pass);  
    LLVMAddGVNPass(jit->pass);  
    LLVMAddCFGSimplificationPass(jit->pass);
    
    /* link in external functions callable from jit'd code */
    llvm_functions(jit);

    /* initialise some strings */
    START_EXEC;
    jit->fmt_str = NULL;
    END_EXEC;

    return jit;
}

/*
   If something goes wrong after partially jit'ing something we need
   to clean up.
*/
void llvm_reset(jit_t * jit)
{
    LLVMDeleteFunction(jit->function);
    LLVMDisposeBuilder(jit->builder);
    jit->function = NULL;
    jit->builder = NULL;
}

/*
   Clean up LLVM on exit from Cesium
*/
void llvm_cleanup(jit_t * jit)
{
    /* Clean up */
    LLVMDisposePassManager(jit->pass);  
    LLVMDisposeExecutionEngine(jit->engine); 
    jit->pass = NULL;
    jit->engine = NULL;
    jit->module = NULL;
    jit->fmt_str = NULL;
    jit->fmt_str = NULL;
}

/*
   Jit an int literal
*/
int exec_int(jit_t * jit, ast_t * ast)
{
    long num = atol(ast->sym->name);
    
    ast->val = LLVMConstInt(LLVMWordType(), num, 0);

    return 0;
}

/*
   We have a number of binary ops we want to jit and they
   all look the same, so define macros for them.
*/
#define exec_binary(__name, __fop, __iop, __str)        \
__name(jit_t * jit, ast_t * ast)                        \
{                                                       \
    ast_t * expr1 = ast->child;                         \
    ast_t * expr2 = expr1->next;                        \
                                                        \
    exec_ast(jit, expr1);                               \
    exec_ast(jit, expr2);                               \
                                                        \
    LLVMValueRef v1 = expr1->val, v2 = expr2->val;      \
                                                        \
    if (expr1->type == t_double)                        \
       ast->val = __fop(jit->builder, v1, v2, __str);   \
    else                                                \
       ast->val = __iop(jit->builder, v1, v2, __str);   \
                                                        \
    ast->type = expr1->type;                            \
                                                        \
    return 0;                                           \
}

/* Jit add, sub, .... ops */
int exec_binary(exec_plus, LLVMBuildFAdd, LLVMBuildAdd, "add")

int exec_binary(exec_minus, LLVMBuildFSub, LLVMBuildSub, "sub")

int exec_binary(exec_times, LLVMBuildFMul, LLVMBuildMul, "times")

int exec_binary(exec_div, LLVMBuildFDiv, LLVMBuildSDiv, "div")

int exec_binary(exec_mod, LLVMBuildFRem, LLVMBuildSRem, "mod")

/* 
   Dispatch to various binary operations 
*/

int exec_binop(jit_t * jit, ast_t * ast)
{
    if (ast->sym == sym_lookup("+"))
        return exec_plus(jit, ast);

    if (ast->sym == sym_lookup("-"))
        return exec_minus(jit, ast);

    if (ast->sym == sym_lookup("*"))
        return exec_times(jit, ast);

    if (ast->sym == sym_lookup("/"))
        return exec_div(jit, ast);

    if (ast->sym == sym_lookup("%"))
        return exec_mod(jit, ast);
}

/*
   As we traverse the ast we dispatch on ast tag to various jit 
   functions defined above
*/
int exec_ast(jit_t * jit, ast_t * ast)
{
    switch (ast->tag)
    {
    case T_INT:
        return exec_int(jit, ast);
    case T_BINOP:
        return exec_binop(jit, ast);
    default:
        return 0;
    }
}

/* We start traversing the ast to do jit'ing */
void exec_root(jit_t * jit, ast_t * ast)
{
    /* Traverse the ast jit'ing everything, then run the jit'd code */
    START_EXEC;
         
    /* jit the ast */
    exec_ast(jit, ast);
    
    /* print the resulting value */
    print_obj(jit, ast->type, ast->val);
    
    END_EXEC;
         
    /* 
       If our jit side print_obj had to create a format string
       clean it up now that we've executed the jit'd code.
    */
    if (jit->fmt_str)
    {
        LLVMDeleteGlobal(jit->fmt_str);
        jit->fmt_str = NULL;
    }
}

