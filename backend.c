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

#define CS_MALLOC "GC_malloc"
#define CS_MALLOC_ATOMIC "GC_malloc_atomic"

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

   /* patch in the exit function */
   args[0] = LLVMWordType();
   ret = LLVMVoidType();
   fntype = LLVMFunctionType(ret, args, 1, 0);
   fn = LLVMAddFunction(jit->module, "exit", fntype);

   /* patch in the GC_malloc function */
   args[0] = LLVMWordType();
   ret = LLVMPointerType(LLVMInt8Type(), 0);
   fntype = LLVMFunctionType(ret, args, 1, 0);
   fn = LLVMAddFunction(jit->module, CS_MALLOC, fntype);
   LLVMAddFunctionAttr(fn, LLVMNoAliasAttribute);

   /* patch in the GC_malloc_atomic function */
   args[0] = LLVMWordType();
   ret = LLVMPointerType(LLVMInt8Type(), 0);
   fntype = LLVMFunctionType(ret, args, 1, 0);
   fn = LLVMAddFunction(jit->module, CS_MALLOC_ATOMIC, fntype);
   LLVMAddFunctionAttr(fn, LLVMNoAliasAttribute);
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

    /* patch in some external functions */
    llvm_functions(jit);

    return jit;
}

/*
   If something goes wrong after partially jit'ing something we need
   to clean up.
*/
void llvm_reset(jit_t * jit)
{
    if (jit->function)
       LLVMDeleteFunction(jit->function);
    if (jit->builder)
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
   Return 1 if type is atomic (i.e. contains no pointers)
*/
int is_atomic(type_t * type)
{
   typ_t t = type->typ;
   return (t != ARRAY && t != TUPLE && t != DATATYPE 
        && t != FN && t != LAMBDA);
}

/* 
   Jit a call to GC_malloc
*/
LLVMValueRef LLVMBuildGCMalloc(jit_t * jit, LLVMTypeRef type, const char * name, int atomic)
{
    LLVMValueRef fn;
    if (atomic)
        fn = LLVMGetNamedFunction(jit->module, CS_MALLOC_ATOMIC);
    else
        fn = LLVMGetNamedFunction(jit->module, CS_MALLOC);
    LLVMValueRef arg[1] = { LLVMSizeOf(type) };
    LLVMValueRef gcmalloc = LLVMBuildCall(jit->builder, fn, arg, 1, "malloc");
    return LLVMBuildPointerCast(jit->builder, gcmalloc, LLVMPointerType(type, 0), name);
}

/* 
   Build llvm struct type from ordinary tuple type
*/
LLVMTypeRef tuple_to_llvm(jit_t * jit, type_t * type)
{
    int params = type->arity;
    int i;

    /* get parameter types */
    LLVMTypeRef * args = (LLVMTypeRef *) GC_MALLOC(params*sizeof(LLVMTypeRef));
    for (i = 0; i < params; i++)
        args[i] = type_to_llvm(jit, type->args[i]); 

    /* make LLVM struct type */
    return LLVMStructType(args, params, 1);
}

/* 
   Convert a type_t to an LLVMTypeRef 
*/
LLVMTypeRef type_to_llvm(jit_t * jit, type_t * type)
{
   if (type == t_nil)
      return LLVMVoidType();
   else if (type == t_int || type == t_uint)
      return LLVMWordType();
   else if (type == t_int8 || type == t_uint8)
      return LLVMInt8Type();
   else if (type == t_int16 || type == t_uint16)
      return LLVMInt16Type();
   else if (type == t_int32 || type == t_uint32)
      return LLVMInt32Type();
   else if (type == t_int64 || type == t_uint64)
      return LLVMInt64Type();
   else if (type == t_double)
      return LLVMDoubleType();
   else if (type == t_float)
      return LLVMFloatType();
   else if (type == t_char)
      return LLVMInt8Type();
   else if (type == t_string)
      return LLVMPointerType(LLVMInt8Type(), 0);
   else if (type == t_bool)
      return LLVMInt1Type();
   else if (type->typ == TUPLE)
      return LLVMPointerType(tuple_to_llvm(jit, type), 0);
   else if (type->typ == DATATYPE)
      return LLVMPointerType(LLVMGetTypeByName(jit->module, type->llvm), 0);
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
    
    LLVMValueRef val = LLVMConstInt(LLVMWordType(), num, 1);

    return ret(0, val);
}

/*
   Jit a uint literal
*/
ret_t * exec_uint(jit_t * jit, ast_t * ast)
{
    unsigned long num = strtoul(ast->sym->name, NULL, 10);
    
    LLVMValueRef val = LLVMConstInt(LLVMWordType(), num, 0);

    return ret(0, val);
}

/*
   Jit a double literal
*/
ret_t * exec_double(jit_t * jit, ast_t * ast)
{
    double num = atof(ast->sym->name);
    
    LLVMValueRef val = LLVMConstReal(LLVMDoubleType(), num);

    return ret(0, val);
}

/*
   Jit a float literal
*/
ret_t * exec_float(jit_t * jit, ast_t * ast)
{
    double num = atof(ast->sym->name);
    
    LLVMValueRef val = LLVMConstReal(LLVMFloatType(), num);

    return ret(0, val);
}

/*
   Jit a char literal
*/
ret_t * exec_char(jit_t * jit, ast_t * ast)
{
    char c = ast->sym->name[0];

    if (c == '\\')
    {
       switch (ast->sym->name[1])
       {
       case '\'':
          c = '\'';
          break;
       case '\"':
          c = '\"';
          break;
       case '\\':
          c = '\\';
          break;
       case '0':
          c = '\0';
          break;
       case 'n':
          c = '\n';
          break;
       case 'r':
          c = '\r';
          break;
       case 't':
          c = '\t';
          break;
       default:
          jit_exception(jit, "Unknown char escape character in exec_char\n");
       }
    }

    LLVMValueRef val = LLVMConstInt(LLVMInt8Type(), c, 0);;

    return ret(0, val);
}

/*
   Jit a string literal
*/
ret_t * exec_string(jit_t * jit, ast_t * ast)
{
    char * str = ast->sym->name;
    
    LLVMValueRef val = LLVMBuildGlobalStringPtr(jit->builder, str, "string");

    return ret(0, val);
}

/*
   We have a number of binary ops we want to jit and they
   all look the same, so define macros for them.
*/
#define exec_binary(__name, __fop, __iop, __str)           \
__name(jit_t * jit, ast_t * ast)                           \
{                                                          \
    ast_t * expr1 = ast->child;                            \
    ast_t * expr2 = expr1->next;                           \
                                                           \
    ret_t * ret1 = exec_ast(jit, expr1);                   \
    ret_t * ret2 = exec_ast(jit, expr2);                   \
                                                           \
    LLVMValueRef v1 = ret1->val, v2 = ret2->val, val;      \
                                                           \
    if (expr1->type == t_double || expr1->type == t_float) \
       val = __fop(jit->builder, v1, v2, __str);           \
    else                                                   \
       val = __iop(jit->builder, v1, v2, __str);           \
                                                           \
    return ret(0, val);                                    \
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
    if (expr1->type == t_double || expr1->type == t_float)            \
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
    
    if (ast->tag == T_BLOCK)
       current_scope = ast->env;
    
    while (c != NULL)
    {
        c_ret = exec_ast(jit, c);
                    
        c = c->next;
    }

    if (ast->tag == T_BLOCK)
       scope_down();

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
   Jit an if..else statement
*/
ret_t * exec_if_else_stmt(jit_t * jit, ast_t * ast)
{
    ast_t * exp = ast->child;
    ast_t * con = exp->next;
    ast_t * alt = con->next;
    
    ret_t * exp_ret;

    LLVMBasicBlockRef i = LLVMAppendBasicBlock(jit->function, "if");
    LLVMBasicBlockRef b1 = LLVMAppendBasicBlock(jit->function, "ifbody");
    LLVMBasicBlockRef b2 = LLVMAppendBasicBlock(jit->function, "elsebody");
    LLVMBasicBlockRef e = LLVMAppendBasicBlock(jit->function, "ifend");

    LLVMBuildBr(jit->builder, i);
    LLVMPositionBuilderAtEnd(jit->builder, i);  
    
    exp_ret = exec_ast(jit, exp); /* expression */

    LLVMBuildCondBr(jit->builder, exp_ret->val, b1, b2);
    LLVMPositionBuilderAtEnd(jit->builder, b1); 
   
    exec_ast(jit, con); /* stmt1 */
    
    LLVMBuildBr(jit->builder, e);

    LLVMPositionBuilderAtEnd(jit->builder, b2);  

    exec_ast(jit, alt); /* stmt2 */
    
    LLVMBuildBr(jit->builder, e);

    LLVMPositionBuilderAtEnd(jit->builder, e); 
      
    return ret(0, NULL);
}

/*
   Jit an if statement
*/
ret_t * exec_if_stmt(jit_t * jit, ast_t * ast)
{
    ast_t * exp = ast->child;
    ast_t * con = exp->next;
    
    ret_t * exp_ret;

    LLVMBasicBlockRef i = LLVMAppendBasicBlock(jit->function, "if");
    LLVMBasicBlockRef b = LLVMAppendBasicBlock(jit->function, "ifbody");
    LLVMBasicBlockRef e = LLVMAppendBasicBlock(jit->function, "ifend");

    LLVMBuildBr(jit->builder, i);
    LLVMPositionBuilderAtEnd(jit->builder, i);  
    
    exp_ret = exec_ast(jit, exp); /* expression */

    LLVMBuildCondBr(jit->builder, exp_ret->val, b, e);
    LLVMPositionBuilderAtEnd(jit->builder, b); 
   
    exec_ast(jit, con); /* stmt1 */
    
    LLVMBuildBr(jit->builder, e);

    LLVMPositionBuilderAtEnd(jit->builder, e); 
      
    return ret(0, NULL);
}

/*
   Jit a while statement
*/
ret_t * exec_while_stmt(jit_t * jit, ast_t * ast)
{
    ast_t * exp = ast->child;
    ast_t * con = exp->next;
    
    ret_t * exp_ret, * con_ret;

    LLVMBasicBlockRef w = LLVMAppendBasicBlock(jit->function, "while");
    LLVMBasicBlockRef b = LLVMAppendBasicBlock(jit->function, "whilebody");
    LLVMBasicBlockRef e = LLVMAppendBasicBlock(jit->function, "whileend");

    LLVMBuildBr(jit->builder, w);
    LLVMPositionBuilderAtEnd(jit->builder, w);  
    
    exp_ret = exec_ast(jit, exp); /* expression */

    LLVMBuildCondBr(jit->builder, exp_ret->val, b, e);
    LLVMPositionBuilderAtEnd(jit->builder, b); 
   
    con_ret = exec_ast(jit, con); /* stmt1 */
    
    if (!con_ret->closed)
        LLVMBuildBr(jit->builder, w);
 
    LLVMPositionBuilderAtEnd(jit->builder, e); 
      
    return ret(0, NULL);
}

ret_t * exec_decl(jit_t * jit, ast_t * ast)
{
   int len = strlen(ast->sym->name);
   char * llvm = GC_MALLOC(len + 12);
   LLVMValueRef val;
   
   strcpy(llvm, ast->sym->name);
   strcpy(llvm + len, serial());
   bind_t * bind = bind_symbol(ast->sym, ast->type, llvm);

   LLVMTypeRef type = type_to_llvm(jit, ast->type); /* convert to llvm type */
       
   if (scope_is_global(bind))
   {
      val = LLVMAddGlobal(jit->module, type, llvm);
      LLVMSetInitializer(val, LLVMGetUndef(type));
   } else
   {
      val = LLVMBuildAlloca(jit->builder, type, llvm);
      bind->llvm_val = val;
   }

   return ret(0, val);
}

/*
   Jit an assignment statement
*/
ret_t * exec_assign(jit_t * jit, ast_t * id, ast_t * expr)
{
    char * llvm;
    ret_t * id_ret, * expr_ret;
    LLVMValueRef val, var;

    bind_t * bind = find_symbol(id->sym);
    if (bind->llvm == NULL) /* symbol doesn't exist yet */
    {
       id_ret = exec_decl(jit, id);
       var = id_ret->val;
    } else
    {
       if (scope_is_global(bind))
          var = LLVMGetNamedGlobal(jit->module, bind->llvm);
       else
          var = bind->llvm_val;
    }

    expr_ret = exec_ast(jit, expr);
    
    LLVMBuildStore(jit->builder, expr_ret->val, var);

    return ret(0, NULL);
}

/*
   Jit a tuple assignment statement
*/
ret_t * exec_tuple_assign(jit_t * jit, ast_t * id, ast_t * expr)
{
    ast_t * a1 = id->child;
    ast_t * a2 = expr->child;
    ret_t * id_ret, * dt_ret;
    LLVMValueRef var;

    int i = 0;
    while (a1 != NULL)
    {
       a1 = a1->next;
       i++;
    }

    LLVMValueRef * vals = GC_MALLOC(i*sizeof(LLVMValueRef));
    i = 0;
    while (a2 != NULL)
    {
       vals[i] = exec_ast(jit, a2)->val;
       a2 = a2->next;
       i++;
    }

    i = 0;
    a1 = id->child;
    while (a1 != NULL)
    {
       if (a1->tag == T_IDENT)
       {
          bind_t * bind = find_symbol(a1->sym);
          if (bind->llvm == NULL) /* symbol doesn't exist yet */
          {
             id_ret = exec_decl(jit, a1);
             var = id_ret->val;
          } else
          {
             if (scope_is_global(bind))
                var = LLVMGetNamedGlobal(jit->module, bind->llvm);
             else
                var = bind->llvm_val;
          }
          LLVMBuildStore(jit->builder, vals[i], var);
       } else if (a1->tag == T_LSLOT)
       {
          dt_ret = exec_ast(jit, a1);
    
          LLVMBuildStore(jit->builder, vals[i], dt_ret->val);
       } else if (a1->tag == T_TUPLE)
          exec_tuple_unpack_val(jit, a1, vals[i], expr->type->args[i]);
       else 
          jit_exception(jit, "Unknown type in exec_tuple_assign\n");

       a1 = a1->next;
       i++;
    }

    return ret(0, NULL);
}

/*
   Jit a tuple unpack from a value
*/
ret_t * exec_tuple_unpack_val(jit_t * jit, ast_t * ast1, LLVMValueRef val, type_t * type)
{
    int i = type->arity;
    LLVMValueRef * vals = GC_MALLOC(i*sizeof(LLVMValueRef));
    ast_t * a1;
    LLVMValueRef var;
    ret_t * id_ret, * dt_ret;

    for (i = 0; i < type->arity; i++)
    {
       LLVMValueRef index[2] = { LLVMConstInt(LLVMInt32Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), i, 0) };
       LLVMValueRef p = LLVMBuildInBoundsGEP(jit->builder, val, index, 2, "tuple");
       LLVMValueRef val = LLVMBuildLoad(jit->builder, p, "entry");
          
       vals[i] = val;
    }

    i = 0;
    a1 = ast1->child;
    while (a1 != NULL)
    {
       if (a1->tag == T_IDENT)
       {
          bind_t * bind = find_symbol(a1->sym);
          if (bind->llvm == NULL) /* symbol doesn't exist yet */
          {
             id_ret = exec_decl(jit, a1);
             var = id_ret->val;
          } else
          {
             if (scope_is_global(bind))
                var = LLVMGetNamedGlobal(jit->module, bind->llvm);
             else
                var = bind->llvm_val;
          }
          LLVMBuildStore(jit->builder, vals[i], var);
       } else if (a1->tag == T_LSLOT)
       {
          dt_ret = exec_ast(jit, a1);
    
          LLVMBuildStore(jit->builder, vals[i], dt_ret->val);
       } else if (a1->tag == T_TUPLE)
          exec_tuple_unpack_val(jit, a1, vals[i], type->args[i]);
       else 
          jit_exception(jit, "Unknown type in exec_tuple_unpack_val\n");

       a1 = a1->next;
       i++;
    }

    return ret(0, NULL);
}

/*
   Jit a tuple unpack
*/
ret_t * exec_tuple_unpack(jit_t * jit, ast_t * ast1, ast_t * ast2)
{
    ret_t * a2_ret = exec_ast(jit, ast2);

    return exec_tuple_unpack_val(jit, ast1, a2_ret->val, ast2->type);
}

/*
   Jit load of an identifier
*/
ret_t * exec_ident(jit_t * jit, ast_t * ast)
{
    bind_t * bind = find_symbol(ast->sym);
    LLVMValueRef var;

    if (scope_is_global(bind))
       var = LLVMGetNamedGlobal(jit->module, bind->llvm);
    else
       var = bind->llvm_val;

    LLVMValueRef val = LLVMBuildLoad(jit->builder, var, bind->llvm);
    
    return ret(0, val);
}

/*
   Jit a tuple expression
*/
ret_t * exec_tuple(jit_t * jit, ast_t * ast)
{
    int params = ast->type->arity;
    int i;
    int atomic = 1;
    ret_t * p_ret;
    LLVMValueRef val;

    LLVMTypeRef tup_tref = tuple_to_llvm(jit, ast->type);

    /* determine whether fields are atomic */
    for (i = 0; i < params; i++)
        atomic &= is_atomic(ast->type->args[i]);

    val = LLVMBuildGCMalloc(jit, tup_tref, "tuple", atomic);

    ast_t * p = ast->child;
    for (i = 0; i < params; i++)
    {
        p_ret = exec_ast(jit, p);
        
         /* insert value into tuple */
        LLVMValueRef indices[2] = { LLVMConstInt(LLVMInt32Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), i, 0) };
        LLVMValueRef entry = LLVMBuildInBoundsGEP(jit->builder, val, indices, 2, "tuple");
        LLVMBuildStore(jit->builder, p_ret->val, entry);
    
        p = p->next;
    }

    return ret(0, val);
}

/*
   Jit a type declaration. Not much we can do except set up 
   the empty struct.
*/
ret_t * exec_type_stmt(jit_t * jit, ast_t * ast)
{
   sym_t * sym = ast->child->sym;
   int len = strlen(sym->name);
   char * llvm = GC_MALLOC(len + 12);
   bind_t * bind;

   strcpy(llvm, sym->name);
   strcpy(llvm + len, serial());
   
   LLVMStructCreateNamed(LLVMGetGlobalContext(), llvm);

   bind = find_symbol(sym);
   bind->type->ret->llvm = llvm;
   
   return ret(0, NULL);
}

/* 
   Jit a function application or type constructor application
*/
ret_t * exec_appl(jit_t * jit, ast_t * ast)
{
   ast_t * id = ast->child;
   ast_t * exp = id->next;

   bind_t * bind = find_symbol(id->sym);

   int i, count = bind->type->ret->arity;

   LLVMValueRef * vals = GC_MALLOC(count*sizeof(LLVMValueRef));
   
   i = 0;
   while (exp != NULL)
   {
      ret_t * r = exec_ast(jit, exp);
      vals[i] = r->val;
      i++;
      exp = exp->next;
   }

   type_t * type = bind->type->ret;
   LLVMTypeRef t = LLVMGetTypeByName(jit->module, type->llvm);
   
   if (bind->llvm == NULL) /* type not yet defined in LLVM */
   {
      LLVMTypeRef * types = GC_MALLOC(count*sizeof(LLVMTypeRef));
      for (i = 0; i < count; i++)
         types[i] = type_to_llvm(jit, type->args[i]);
      LLVMStructSetBody(t, types, count, 0);
      bind->llvm = type->llvm;
   }

   /* determine whether types are atomic */
   int atomic = 1;
   for (i = 0; i < count; i++)
        atomic &= is_atomic(type->args[i]);

   LLVMValueRef val = LLVMBuildGCMalloc(jit, t, type->sym->name, atomic);

   for (i = 0; i < count; i++)
   {
       /* insert value into datatype */
       LLVMValueRef indices[2] = { LLVMConstInt(LLVMInt32Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), i, 0) };
       LLVMValueRef entry = LLVMBuildInBoundsGEP(jit->builder, val, indices, 2, type->sym->name);
       LLVMBuildStore(jit->builder, vals[i], entry);
   } 

   return ret(0, val);
}

/*
   Jit a load from a slot
*/
ret_t * exec_slot(jit_t * jit, ast_t * ast)
{
   ast_t * dt = ast->child;
   ast_t * slot = dt->next;

   ret_t * r = exec_ast(jit, dt);
   type_t * type = dt->type;
   int i;

   for (i = 0; i < type->arity; i++)
      if (type->slots[i] == slot->sym)
            break;

   LLVMValueRef index[2] = { LLVMConstInt(LLVMInt32Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), i, 0) };
   LLVMValueRef p = LLVMBuildInBoundsGEP(jit->builder, r->val, index, 2, "datatype");
   LLVMValueRef val = LLVMBuildLoad(jit->builder, p, "slot");
   
   return ret(0, val);
}

/*
   Jit a slot assignment statement
*/
ret_t * exec_slot_assign(jit_t * jit, ast_t * ast)
{
    ast_t * dt = ast->child;
    ast_t * expr = dt->next;
    ret_t * dt_ret, * expr_ret;
    LLVMValueRef val;

    expr_ret = exec_ast(jit, expr);
    dt_ret = exec_ast(jit, dt);
    
    LLVMBuildStore(jit->builder, expr_ret->val, dt_ret->val);

    return ret(0, NULL);
}
/*
   Jit access to a slot as a place
*/
ret_t * exec_lslot(jit_t * jit, ast_t * ast)
{
   ast_t * dt = ast->child;
   ast_t * slot = dt->next;

   ret_t * r = exec_ast(jit, dt);
   type_t * type = dt->type;
   int i;

   for (i = 0; i < type->arity; i++)
      if (type->slots[i] == slot->sym)
            break;

   LLVMValueRef index[2] = { LLVMConstInt(LLVMInt32Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), i, 0) };
   LLVMValueRef val = LLVMBuildInBoundsGEP(jit->builder, r->val, index, 2, "datatype");
   
   return ret(0, val);
}

/*
   Jit a function statement. We don't jit the
   function until it is actually called the first time.
*/
ret_t * exec_fn_stmt(jit_t * jit, ast_t * ast)
{
   return ret(0, NULL);
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
    case T_UINT:
        return exec_uint(jit, ast);
    case T_DOUBLE:
        return exec_double(jit, ast);
    case T_FLOAT:
        return exec_float(jit, ast);
    case T_CHAR:
        return exec_char(jit, ast);
    case T_STRING:
        return exec_string(jit, ast);
    case T_TUPLE:
        return exec_tuple(jit, ast);
    case T_BINOP:
        return exec_binop(jit, ast);
    case T_IF_ELSE_EXPR:
        return exec_if_else_expr(jit, ast);
    case T_IF_ELSE_STMT:
        return exec_if_else_stmt(jit, ast);
    case T_IF_STMT:
        return exec_if_stmt(jit, ast);
    case T_WHILE_STMT:
        return exec_while_stmt(jit, ast);
    case T_BLOCK:
    case T_THEN:
    case T_ELSE:
    case T_DO:
        return exec_block(jit, ast);
    case T_TYPE_STMT:
        return exec_type_stmt(jit, ast);
    case T_ASSIGN:
        return exec_assign(jit, ast->child, ast->child->next);
    case T_TUPLE_ASSIGN:
        return exec_tuple_assign(jit, ast->child, ast->child->next);
    case T_TUPLE_UNPACK:
        return exec_tuple_unpack(jit, ast->child, ast->child->next);
    case T_SLOT_ASSIGN:
        return exec_slot_assign(jit, ast);
    case T_IDENT:
        return exec_ident(jit, ast);
    case T_APPL:
        return exec_appl(jit, ast);
    case T_SLOT:
        return exec_slot(jit, ast);
    case T_LSLOT:
        return exec_lslot(jit, ast);
    case T_FN_STMT:
        return exec_fn_stmt(jit, ast);
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
   Print the given entry of a struct
*/
void print_struct_entry(jit_t * jit, type_t * type, int i, LLVMGenericValueRef val)
{
   type_t * t = type->args[i];
   LLVMTypeRef lt = type_to_llvm(jit, t);
   LLVMTypeRef ltype = type_to_llvm(jit, type);
   LLVMGenericValueRef gen_val;
   
   LLVMBuilderRef builder = LLVMCreateBuilder();
   LLVMTypeRef args[1] = { ltype };
   LLVMTypeRef fn_type = LLVMFunctionType(lt, args, 1, 0);
   LLVMValueRef function = LLVMAddFunction(jit->module, "exec2", fn_type);
   LLVMBasicBlockRef entry = LLVMAppendBasicBlock(function, "entry");
   LLVMPositionBuilderAtEnd(builder, entry);
   
   LLVMValueRef obj = LLVMGetParam(function, 0);
   LLVMValueRef index[2] = { LLVMConstInt(LLVMInt32Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), i, 0) };
   LLVMValueRef p = LLVMBuildInBoundsGEP(builder, obj, index, 2, "tuple");
   LLVMValueRef res = LLVMBuildLoad(builder, p, "entry");
    
   if (t == t_nil)
      LLVMBuildRetVoid(builder);
   else
      LLVMBuildRet(builder, res);

   LLVMRunFunctionPassManager(jit->pass, function);
   LLVMGenericValueRef exec_args[1] = { val };
   gen_val = LLVMRunFunction(jit->engine, function, 1, exec_args);
   
   LLVMDeleteFunction(function);
   LLVMDisposeBuilder(builder);

   print_gen(jit, t, gen_val);
}

/*
   Print special characters in character format
*/
int print_special(char c)
{
   switch (c)
   {
   case '\'':
      printf("'\\''");
      return 1;
   case '\"':
      printf("'\\\"'");
      return 1;
   case '\\':
      printf("'\\\\'");
      return 1;
   case '\0':
      printf("'\\0'");
      return 1;
   case '\n':
      printf("'\\n'");
      return 1;
   case '\r':
      printf("'\\r'");
      return 1;
   case '\t':
      printf("'\\t'");
      return 1;
   default:
      return 0;
   }
}

/*
   Print the generic return value from exec
*/
void print_gen(jit_t * jit, type_t * type, LLVMGenericValueRef gen_val)
{
   int i, res;
   
   if (type == t_nil)
      printf("none");
   else if (type == t_int || type == t_int8 || type == t_int16
       || type == t_int32 || type == t_int64)
      printf("%ld", (long) LLVMGenericValueToInt(gen_val, 1));
   else if (type == t_uint || type == t_uint8 || type == t_uint16
       || type == t_uint32 || type == t_uint64)
      printf("%luu", (unsigned long) LLVMGenericValueToInt(gen_val, 0));
   else if (type == t_double)
      printf("%lg", (double) LLVMGenericValueToFloat(LLVMDoubleType(), gen_val));
   else if (type == t_float)
      printf("%gf", (float) LLVMGenericValueToFloat(LLVMFloatType(), gen_val));
   else if (type == t_char)
   {
      char c = (char) LLVMGenericValueToInt(gen_val, 0);
      if (!print_special(c))
         printf("'%c'", c);
   }
   else if (type == t_string)
      printf("\"%s\"", (char *) LLVMGenericValueToPointer(gen_val));
   else if (type == t_bool)
   {
      if (LLVMGenericValueToInt(gen_val, 0))
         printf("true");
      else
         printf("false");
   } else if (type->typ == TUPLE)
   {
      printf("(");
      for (i = 0; i < type->arity - 1; i++)
          print_struct_entry(jit, type, i, gen_val), printf(", ");
      print_struct_entry(jit, type, i, gen_val);
      if (type->arity == 1)
         printf(",");
      printf(")");
   } else if (type->typ == DATATYPE)
   {
      printf("%s(", type->sym->name);
      for (i = 0; i < type->arity - 1; i++)
          print_struct_entry(jit, type, i, gen_val), printf(", ");
      print_struct_entry(jit, type, i, gen_val);
      printf(")");
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
    print_gen(jit, ast->type, gen_val), printf("\n");
}

