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
}

/* 
   Raise an exception but clean up jit first 
*/
void jit_exception(jit_t * jit, const char * msg)
{
   llvm_reset(jit);
   
   exception(msg);
}

/* 
   Convert a type_t to an LLVMTypeRef 
*/
LLVMTypeRef type_to_llvm(jit_t * jit, type_t * type)
{
   if (type == t_nil)
      return LLVMVoidType();
   else if (type == t_int)
      return LLVMWordType();
   else if (type == t_bool)
      return LLVMInt1Type();
   else
      jit_exception(jit, "Unknown type in type_to_llvm\n");
}

/*
   Create a return struct for return from jit'ing an AST node
*/
ret_t * ret(int closed, LLVMValueRef val)
{
   ret_t * ret = GC_MALLOC(sizeof(ret_t));
   ret->closed = closed;
   ret->val = val;
   return ret;
}

/*
   Jit an int literal
*/
ret_t * exec_int(jit_t * jit, ast_t * ast)
{
    long num = atol(ast->sym->name);
    
    LLVMValueRef val = LLVMConstInt(LLVMWordType(), num, 0);

    return ret(0, val);
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
    ret_t * ret1 = exec_ast(jit, expr1);                \
    ret_t * ret2 = exec_ast(jit, expr2);                \
                                                        \
    LLVMValueRef v1 = ret1->val, v2 = ret2->val, val;   \
                                                        \
    if (expr1->type == t_double)                        \
       val = __fop(jit->builder, v1, v2, __str);        \
    else                                                \
       val = __iop(jit->builder, v1, v2, __str);        \
                                                        \
    return ret(0, val);                                 \
}

/* 
   Jit add, sub, .... ops 
*/
ret_t * exec_binary(exec_plus, LLVMBuildFAdd, LLVMBuildAdd, "add")

ret_t * exec_binary(exec_minus, LLVMBuildFSub, LLVMBuildSub, "sub")

ret_t * exec_binary(exec_times, LLVMBuildFMul, LLVMBuildMul, "times")

ret_t * exec_binary(exec_div, LLVMBuildFDiv, LLVMBuildSDiv, "div")

ret_t * exec_binary(exec_mod, LLVMBuildFRem, LLVMBuildSRem, "mod")

/*
   We have a number of binary rels we want to jit and they
   all look the same, so define macros for them.
*/

#define exec_binary_rel(__name, __fop, __frel, __iop, __irel, __str)  \
__name(jit_t * jit, ast_t * ast)                                      \
{                                                                     \
    ast_t * expr1 = ast->child;                                       \
    ast_t * expr2 = expr1->next;                                      \
                                                                      \
    ret_t * ret1 = exec_ast(jit, expr1);                              \
    ret_t * ret2 = exec_ast(jit, expr2);                              \
                                                                      \
    LLVMValueRef v1 = ret1->val, v2 = ret2->val, val;                 \
                                                                      \
    if (expr1->type == t_double)                                      \
       val = __fop(jit->builder, __frel, v1, v2, __str);              \
    else                                                              \
       val = __iop(jit->builder, __irel, v1, v2, __str);              \
                                                                      \
    return ret(0, val);                                               \
}

/* 
   Jit eq, neq, lt, gt, .... rels 
*/

ret_t * exec_binary_rel(exec_le, LLVMBuildFCmp, LLVMRealOLE, LLVMBuildICmp, LLVMIntSLE, "le")

ret_t * exec_binary_rel(exec_ge, LLVMBuildFCmp, LLVMRealOGE, LLVMBuildICmp, LLVMIntSGE, "ge")

ret_t * exec_binary_rel(exec_lt, LLVMBuildFCmp, LLVMRealOLT, LLVMBuildICmp, LLVMIntSLT, "lt")

ret_t * exec_binary_rel(exec_gt, LLVMBuildFCmp, LLVMRealOGT, LLVMBuildICmp, LLVMIntSGT, "gt")

ret_t * exec_binary_rel(exec_eq, LLVMBuildFCmp, LLVMRealOEQ, LLVMBuildICmp, LLVMIntEQ, "eq")

ret_t * exec_binary_rel(exec_ne, LLVMBuildFCmp, LLVMRealONE, LLVMBuildICmp, LLVMIntNE, "ne")

/* 
   Dispatch to various binary operations 
*/

ret_t * exec_binop(jit_t * jit, ast_t * ast)
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

    if (ast->sym == sym_lookup("=="))
        return exec_eq(jit, ast);

    if (ast->sym == sym_lookup("!="))
        return exec_ne(jit, ast);

    if (ast->sym == sym_lookup("<="))
        return exec_le(jit, ast);

    if (ast->sym == sym_lookup(">="))
        return exec_ge(jit, ast);

    if (ast->sym == sym_lookup("<"))
        return exec_lt(jit, ast);

    if (ast->sym == sym_lookup(">"))
        return exec_gt(jit, ast);

    jit_exception(jit, "Unknown symbol in binop\n");
}

/*
   Jit a block of statements
*/
ret_t * exec_block(jit_t * jit, ast_t * ast)
{
    ast_t * c = ast->child;
    ret_t * c_ret;
    
    while (c != NULL)
    {
        c_ret = exec_ast(jit, c);
                    
        c = c->next;
    }

    return c_ret;
}

/*
   Jit an if..else expression
*/
ret_t * exec_if_else_expr(jit_t * jit, ast_t * ast)
{
    ast_t * exp = ast->child;
    ast_t * con = exp->next;
    ast_t * alt = con->next;
    
    ret_t * exp_ret, * con_ret, * alt_ret;
    LLVMValueRef val;

    LLVMBasicBlockRef i = LLVMAppendBasicBlock(jit->function, "if");
    LLVMBasicBlockRef b1 = LLVMAppendBasicBlock(jit->function, "ifbody");
    LLVMBasicBlockRef b2 = LLVMAppendBasicBlock(jit->function, "elsebody");
    LLVMBasicBlockRef e = LLVMAppendBasicBlock(jit->function, "ifend");

    LLVMBuildBr(jit->builder, i);
    LLVMPositionBuilderAtEnd(jit->builder, i);  
    
    exp_ret = exec_ast(jit, exp); /* expression */

    LLVMValueRef tmp = LLVMBuildAlloca(jit->builder, type_to_llvm(jit, ast->type), "ifexpr");
    
    LLVMBuildCondBr(jit->builder, exp_ret->val, b1, b2);
    LLVMPositionBuilderAtEnd(jit->builder, b1); 
   
    con_ret = exec_ast(jit, con); /* stmt1 */
    LLVMBuildStore(jit->builder, con_ret->val, tmp);

    LLVMBuildBr(jit->builder, e);

    LLVMPositionBuilderAtEnd(jit->builder, b2);  

    alt_ret = exec_ast(jit, alt); /* stmt2 */
    LLVMBuildStore(jit->builder, alt_ret->val, tmp);

    LLVMBuildBr(jit->builder, e);

    LLVMPositionBuilderAtEnd(jit->builder, e); 
    
    val = LLVMBuildLoad(jit->builder, tmp, "if_else_tmp");
      
    return ret(0, val);
}

/*
   As we traverse the ast we dispatch on ast tag to various jit 
   functions defined above
*/
ret_t * exec_ast(jit_t * jit, ast_t * ast)
{
    switch (ast->tag)
    {
    case T_INT:
        return exec_int(jit, ast);
    case T_BINOP:
        return exec_binop(jit, ast);
    case T_IF_ELSE_EXPR:
        return exec_if_else_expr(jit, ast);
    case T_BLOCK:
    case T_THEN:
    case T_ELSE:
        return exec_block(jit, ast);
    default:
        jit_exception(jit, "Unknown AST tag in exec_ast\n");
    }
}

/* 
   Jit a return
*/
void exec_return(jit_t * jit, ast_t * ast, LLVMValueRef val)
{
   if (ast->type == t_nil)
      LLVMBuildRetVoid(jit->builder);
   else
      LLVMBuildRet(jit->builder, val);
}

/*
   Print the generic return value from exec
*/
void print_gen(type_t * type, LLVMGenericValueRef gen_val)
{
   int res;
   
   if (type == t_nil)
      printf("None\n");
   else if (type == t_int)
      printf("%ld\n", (long) LLVMGenericValueToInt(gen_val, 1));
   else if (type == t_bool)
   {
      if (LLVMGenericValueToInt(gen_val, 0))
         printf("true\n");
      else
         printf("false\n");
   } else
      exception("Unknown type in print_gen\n");
}

/* 
   We start traversing the ast to do jit'ing 
*/
void exec_root(jit_t * jit, ast_t * ast)
{
    LLVMGenericValueRef gen_val;
    ret_t * ret;

    /* Traverse the ast jit'ing everything, then run the jit'd code */
    START_EXEC(type_to_llvm(jit, ast->type));
         
    /* jit the ast */
    ret = exec_ast(jit, ast);

    /* jit the return statement for the exec function */
    exec_return(jit, ast, ret->val);
    
    /* get the generic return value from exec */
    END_EXEC(gen_val);

    /* print the resulting value */
    print_gen(ast->type, gen_val);
}

