#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"

#include <unordered_map>
#include <unordered_set>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <memory>
#include <map>

using std::unordered_set;
using std::unordered_map;
using std::string, std::to_string; 
using std::vector; 
using std::pair;
using std::map;

using std::ifstream;
using std::cerr; 
using std::cout;
using std::cin; 

using std::make_unique;
using std::make_shared;
using std::unique_ptr;
using std::shared_ptr;

using ll = long long;

using namespace llvm;
using namespace llvm::sys;


// * Headers
#include "headers/token.hpp"
#include "headers/ast.hpp"

#ifdef DEBUG_MODE   
    #define debug_tok(tok) cout << "Type: " << (tok) << ". "\
        << "Ivalue: " << TokInt << ". "\
        << "Value: '" << TokWord << "'. "\
        << "Line: " << TokLine << ".\n";
    #define debug() cout << "Сейчас здесь: " << __FUNCTION__ << " " << __LINE__ << ".\n";
#else
    #define debug_tok(tok) ;
    #define debug() ;
    #define scream() ;
#endif


// * Lexer
static string compileFile; // has init in main
static int file_size; // has init in main

static string TokWord;
static ll TokInt = 0;
static int TokLine = 1;
static int i = 0;

TOKEN getNextToken() {
    TokWord = ""; TokInt = 0;

    if(i == file_size) {
        return TOKEN::EOFILE;
    } else if(isalpha(compileFile[i])) {
        for(; isalnum(compileFile[i]) && i < file_size; ++i) 
            TokWord += compileFile[i];

        if(tokenTable.count(TokWord))
            return tokenTable[TokWord];
        else
            return TOKEN::IDENTIFIER;
    } else if(isdigit(compileFile[i])) {
        for(; !isspace(compileFile[i]) && !ispunct(compileFile[i]) && i < file_size; ++i) {
            if(isalpha(compileFile[i])) {
                cerr << "Syntax Error: invalid identifier\n";
                return TOKEN::UNDEFINED;
            }

            TokWord += compileFile[i];
        }
        TokInt = std::stoll(TokWord);
        return TOKEN::INTEGER;
    } else if(isspace(compileFile[i])) {
        if(compileFile[i] == '\n') ++TokLine;
        ++i;
        return getNextToken();
    } else if(compileFile[i] == '"') {
        ++i;
        for(; compileFile[i] != '"' && i < file_size; ++i)
            TokWord += compileFile[i];
        
        if(i == file_size && compileFile[i] != '"') {
            cerr << "SyntaxError: excepted '" << '"' << "'\n";
            return TOKEN::UNDEFINED;
        }

        return TOKEN::STRING;
    } else if(ispunct(compileFile[i])) {
        TokWord += compileFile[i++];

        if(!tokenTable.count(TokWord)) {
            cerr << "SyntaxError: unknown punct\n";
            return TOKEN::UNDEFINED;
        } else if((int)tokenTable[TokWord] < 29 && (int)tokenTable[TokWord] > 32)
            return tokenTable[TokWord];
        
        if(compileFile[i] == '=')
            TokWord += compileFile[i++];
        
        return tokenTable[TokWord];
    }

    cerr << "SyntaxError: unknown token\n";
    return TOKEN::UNDEFINED;
}

// * Parser

// TOKENI
static TOKEN CurrTok; // has init in main
static TOKEN skipToken() {
    return CurrTok = getNextToken(); 
}

// SCOUPI

static shared_ptr<Scope> CurrScope = make_shared<Scope>("global", nullptr);

static shared_ptr<Scope> newScope(const string& scopeName = "scope") {
    shared_ptr<Scope> NewScope = make_shared<Scope>(scopeName, CurrScope);

    return CurrScope = NewScope;
}

static shared_ptr<Scope> getOutScope() {
    return CurrScope = CurrScope->getPrev();
}


unique_ptr<StmtAST> LogStmtError(const string& msg) {
    cerr << "Syntax Error: " << msg << ". Line: " << TokLine << "\n";
    return nullptr;
}

unique_ptr<ExprAST> LogExprError(const string& msg) {
    LogStmtError(msg);
    return nullptr;
}

unique_ptr<InputAST> LogInputError(const string& msg) {
    LogStmtError(msg);
    return nullptr;
}

unique_ptr<StmtAST> ParseStatement()
                , ParseIfStmt()
                , ParseWhileStmt()
                , ParseVarStmt()
                , ParseFuncStmt()
                , ParseRetStmt()
                , ParseParenStmts()
                , ParseHighLevelExpr();

unique_ptr<ExprAST> ParseExpression()
                , ParseBoolExpr()
                , ParseIntExpr()
                , ParseTerm()
                , ParseFactor();

unique_ptr<InputAST> Input() {
    vector<unique_ptr<StmtAST>> stmts;

    while(CurrTok != TOKEN::EOFILE) {
        unique_ptr<StmtAST> stmt = ParseStatement();

        if(!stmt)
            return nullptr;

        stmts.push_back(std::move(stmt));

        if(CurrTok != TOKEN::SEMICOL && CurrTok != TOKEN::RBRA)
            return LogInputError("missing semicol ';'");
        
        skipToken(); // eat semicol or rbra
    }
    
    return make_unique<InputAST>(std::move(stmts));
}

unique_ptr<StmtAST> ParseStatement() {
    switch(CurrTok) {
        case TOKEN::IF: return ParseIfStmt();
        case TOKEN::WHILE: return ParseWhileStmt();
        case TOKEN::VAR: return ParseVarStmt();
        case TOKEN::FUNC: return ParseFuncStmt();
        case TOKEN::RETURN: return ParseRetStmt();
        case TOKEN::LBRA: return ParseParenStmts();
        default: return ParseHighLevelExpr();
    }

    return nullptr;
}

unique_ptr<StmtAST> ParseHighLevelExpr() {
    unique_ptr<ExprAST> expr = ParseExpression();
    if(!expr)
        return nullptr;

    return make_unique<HighLevelExpr>(std::move(expr));
}

unique_ptr<StmtAST> ParseIfStmt() {
    skipToken(); // eat if-token
    if(CurrTok != TOKEN::LBRACE)
        return LogStmtError("missing left brace '['");
    
    skipToken();
    unique_ptr<ExprAST> cond = ParseExpression();

    if(!cond)
        return nullptr;

    if(CurrTok != TOKEN::RBRACE)
        return LogStmtError("missing right brace ']'");
    
    skipToken(); // eat rbrace
    if(CurrTok != TOKEN::DO)
        return LogStmtError("not found do-token");
    

    skipToken(); // eat do-token
    unique_ptr<StmtAST> body = ParseStatement();

    if(!body)
        return nullptr;
    
    return make_unique<IfStmt>(std::move(cond), std::move(body));
}

unique_ptr<StmtAST> ParseWhileStmt() {
    skipToken(); // eat while
    if(CurrTok != TOKEN::LBRACE)
        return LogStmtError("missing lbrace '['");

    skipToken();
    unique_ptr<ExprAST> cond = ParseExpression();

    if(!cond)
        return nullptr;

    if(CurrTok != TOKEN::RBRACE)
        return LogStmtError("missing rbrace ']'");

    skipToken(); // eat rbrace
    unique_ptr<StmtAST> body = ParseStatement();

    if(!body)
        return nullptr;
    
    return make_unique<WhileStmt>(std::move(cond), std::move(body));
}

unique_ptr<StmtAST> ParseVarStmt() {
    skipToken(); // eat var

    if(CurrTok != TOKEN::IDENTIFIER)
        return LogStmtError("invalid identifier");

    string varName = TokWord;

    skipToken(); // eat id
    if(CurrTok != TOKEN::ASSIGN)
        return LogStmtError("excepted '='");
    
    skipToken();
    unique_ptr<ExprAST> expr = ParseExpression();

    if(!expr)
        return nullptr;

    CurrScope->add(varName);
    
    return make_unique<VarStmt>(varName, std::move(expr));
}

unique_ptr<StmtAST> ParseFuncStmt() {
    skipToken(); // eat func-token

    if(CurrTok != TOKEN::IDENTIFIER)
        return LogStmtError("invalid identifier");
    
    string Name = TokWord;

    CurrScope->add(Name);

    skipToken(); // eat id
    if(CurrTok != TOKEN::LBRACE)
        return LogStmtError("excepted '['");


    vector<string> args;
    newScope(Name);

    skipToken(); // eat arg id
    while(CurrTok != TOKEN::RBRACE) {
        if(CurrTok != TOKEN::IDENTIFIER)
            return LogStmtError("invalid argument");
        
        args.push_back(TokWord);
        CurrScope->add(TokWord);

        skipToken();
        if(CurrTok != TOKEN::RBRACE && CurrTok != TOKEN::COMMA)
            return LogStmtError("invalid arguments sequence");
        else if(CurrTok == TOKEN::COMMA)
            skipToken(); // eat comma
    }

    skipToken(); // eat rbrace

    unique_ptr<StmtAST> body = ParseStatement();

    if(!body) {
        CurrScope->remove(Name);
        return nullptr;
    }
    getOutScope();

    return make_unique<FuncStmt>(Name, std::move(args), std::move(body));
}

unique_ptr<StmtAST> ParseRetStmt() {
    skipToken(); // eat return

    unique_ptr<ExprAST> expr = ParseExpression();

    if(!expr)
        return nullptr;
    
    return make_unique<RetStmt>(std::move(expr));
}

unique_ptr<StmtAST> ParseParenStmts() {
    skipToken(); // eat lbra

    vector<unique_ptr<StmtAST>> stmts;
    
    newScope(); // change currscope

    while(CurrTok != TOKEN::RBRA) {
        unique_ptr<StmtAST> stmt = ParseStatement();
        
        if(!stmt)
            return nullptr;

        if(CurrTok != TOKEN::SEMICOL && CurrTok != TOKEN::RBRA)
            return LogStmtError("excepted ';'");

        skipToken();
        stmts.push_back(std::move(stmt));
    }

    getOutScope();

    return make_unique<ParenStmts>(std::move(stmts));
}

unique_ptr<ExprAST> ParseExpression() {
    unique_ptr<ExprAST> lhs = ParseBoolExpr();

    if(!lhs)
        return nullptr;

    if(CurrTok != TOKEN::ASSIGN)
        return std::move(lhs);
    
    skipToken(); // eat assign

    if(!lhs->isLvalue)
        return LogExprError("is not lvalue expression");

    if(!CurrScope->find(lhs->getName()))
        return LogExprError("unknown lvalue");

    unique_ptr<ExprAST> rhs = ParseExpression();

    if(!rhs)
        return nullptr;

    return make_unique<AssignExpr>(std::move(lhs), std::move(rhs));
}

unique_ptr<ExprAST> ParseBoolExpr() {
    unique_ptr<ExprAST> lhs = ParseIntExpr();

    if(!lhs)
        return nullptr;
    
    while(true) {
        if((int)CurrTok < 24 || (int)CurrTok > 29)
            return std::move(lhs);

        string Op = TokWord;

        skipToken(); // eat operand
        unique_ptr<ExprAST> rhs = ParseIntExpr();

        if(!rhs)
            return nullptr;

        lhs = make_unique<BoolExpr>(Op, std::move(lhs), std::move(rhs));
    }

    return LogExprError("invalid expr parsing");
}

unique_ptr<ExprAST> ParseIntExpr() {
    unique_ptr<ExprAST> lhs = ParseTerm();

    if(!lhs)
        return nullptr;
    
    while(true) {
        if((int)CurrTok < 18 || (int)CurrTok > 19)
            return std::move(lhs);
        
        string Op = TokWord;

        skipToken(); // eat operand
        unique_ptr<ExprAST> rhs = ParseTerm();

        if(!rhs)
            return nullptr;
        
        lhs = make_unique<IntExpr>(Op, std::move(lhs), std::move(rhs));
    }

    return LogExprError("invalid expr parsing");
}


unique_ptr<ExprAST> ParseTerm() {
    unique_ptr<ExprAST> lhs = ParseFactor();

    if(!lhs)
        return nullptr;
    
    while(true) {
        if((int)CurrTok < 20 || (int)CurrTok > 21)
            return std::move(lhs);
        
        string Op = TokWord;

        skipToken(); // eat operand
        unique_ptr<ExprAST> rhs = ParseFactor();

        if(!rhs)
            return nullptr;
        
        lhs = make_unique<TermExpr>(Op, std::move(lhs), std::move(rhs));
    }

    return LogExprError("invalid expr parsing");
}


unique_ptr<ExprAST> ParseFactor() {
    TOKEN PrevTok = CurrTok;
    string Name = TokWord;
    ll ivalue = TokInt;

    skipToken();
    switch(PrevTok) {
        case TOKEN::INTEGER: return make_unique<NumberExpr>(ivalue);
        case TOKEN::IDENTIFIER: {
            if(!CurrScope->find(Name))
                return LogExprError("not declarated identifier");

            if(CurrTok == TOKEN::LBRACE) {
                skipToken(); // eat lbrace
                vector<unique_ptr<ExprAST>> args;

                while(CurrTok != TOKEN::RBRACE) {
                    unique_ptr<ExprAST> expr = ParseExpression();

                    if(!expr)
                        return nullptr;

                    args.push_back(std::move(expr));

                    if(CurrTok != TOKEN::RBRACE && CurrTok != TOKEN::COMMA)
                        return LogExprError("excepted ']'");
                    else if(CurrTok == TOKEN::COMMA)
                        skipToken(); // eat comma
                }
                skipToken(); // eat rbrace

                return make_unique<CallExpr>(Name, std::move(args));
            } else
                return make_unique<IDExpr>(Name);
        }
        case TOKEN::LBAR: {
            unique_ptr<ExprAST> expr = ParseExpression();

            if(!expr)
                return nullptr;

            debug_tok(CurrTok)

            if(CurrTok != TOKEN::RBAR)
                return LogExprError("excepted ')'");

            skipToken();
            return std::move(expr);
        }
        default: return LogExprError("not valid factor");
    }

    return nullptr;
}
// * Codegen part

static unique_ptr<LLVMContext> TheContext;
static unique_ptr<Module> TheModule;
static unique_ptr<IRBuilder<>> Builder;
static map<string, AllocaInst *> NamedValues;

static void InitializeModule() {
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("my cool jit", *TheContext);
    Builder = std::make_unique<IRBuilder<>>(*TheContext);
}

Value *LogGenError(const string& msg) {
    cerr << "Compile Error: " << msg << '\n';
    return nullptr;
}

Value *InputAST::codegen() {
    for(size_t i = 0, e = statements.size(); i != e; ++i) {
        Value *stmtV = statements[i]->codegen();

        if(!stmtV)
            return nullptr;
    }

    return ConstantInt::get(*TheContext, APInt(64, 0, true));
}

Value *NumberExpr::codegen() {
    return ConstantInt::get(*TheContext, APInt(64, ivalue, true));
}

Value *IDExpr::codegen() {
    AllocaInst *V = NamedValues[name];
    if(!V)
        return LogGenError("unknown variable");

    Value *Val = Builder->CreateLoad(Type::getInt64Ty(*TheContext), V, name);
    
    return Val;
}

Value *CallExpr::codegen() {
    Function *CallFunc = TheModule->getFunction(name);
    if(!CallFunc)
        return LogGenError("Unknown function referenced");
    
    if(CallFunc->arg_size() != args.size())
        return LogGenError("Incorrect # arguments passed");
    
    vector<Value *> ArgsV;
    for(size_t i = 0, e = args.size(); i != e; ++i) {
        ArgsV.push_back(args[i]->codegen());
        if(!ArgsV.back())
            return nullptr;
    }

    return Builder->CreateCall(CallFunc, ArgsV, "calltmp");
}

Function *FuncStmt::codegen() {
    BasicBlock *PredBB = Builder->GetInsertBlock();

    vector<Type *> Ints(args.size(), Type::getInt64Ty(*TheContext));
    FunctionType *FT =
        FunctionType::get(Type::getInt64Ty(*TheContext), Ints, false);

    Function *F =
        Function::Create(FT, Function::ExternalLinkage, name, TheModule.get());
    
    size_t Idx = 0;
    for (auto &Arg : F->args())
        Arg.setName(args[Idx++]);

    BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", F);
    Builder->SetInsertPoint(BB);

    map<string, AllocaInst *> OldBinds;

    for (auto &Arg : F->args()) {
        string ArgName = string(Arg.getName());

        if(NamedValues.count(ArgName))
            OldBinds[ArgName] = NamedValues[ArgName];

        AllocaInst *ArgAlloca = Builder->CreateAlloca(Type::getInt64Ty(*TheContext), nullptr, ArgName);

        Builder->CreateStore(&Arg, ArgAlloca);

        NamedValues[ArgName] = ArgAlloca;
    }
    
    Value *BodyV = body->codegen();

    if(!BodyV)
        return nullptr;

    for(auto [N, Val]: OldBinds)
        NamedValues[N] = Val;

    verifyFunction(*F);
    
    Builder->SetInsertPoint(PredBB);

    return F;
}

Value *VarStmt::codegen() {
    Value *V = Val->codegen();
    if(!V)
        return nullptr;

    AllocaInst *Alloca = Builder->CreateAlloca(Type::getInt64Ty(*TheContext), nullptr, VarName);

    NamedValues[VarName] = Alloca;

    Builder->CreateStore(V, Alloca);

    return ConstantInt::get(*TheContext, APInt(64, 1, true));
}

Value *IfStmt::codegen() {
    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    Value *CondV = Cond->codegen();
    if(!CondV)
        return nullptr;

    BasicBlock *BodyBB = BasicBlock::Create(*TheContext, "ifbody", TheFunction);
    BasicBlock *nextBB = BasicBlock::Create(*TheContext, "next", TheFunction);
    
    Builder->CreateCondBr(CondV, BodyBB, nextBB);

    Builder->SetInsertPoint(BodyBB);

    Value *BodyV = Body->codegen();
    if(!BodyV)
        return nullptr;

    Builder->CreateBr(nextBB);

    Builder->SetInsertPoint(nextBB);

    return ConstantInt::get(*TheContext, APInt(64, 0, true));
}

Value *WhileStmt::codegen() {
    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    BasicBlock *CondBB = BasicBlock::Create(*TheContext, "whilecondbr", TheFunction);

    Builder->CreateBr(CondBB);

    Builder->SetInsertPoint(CondBB);

    Value *CondV = Cond->codegen();
    if(!CondV)
        return nullptr;


    BasicBlock *BodyBB = BasicBlock::Create(*TheContext, "whilebody", TheFunction);
    BasicBlock *nextBB = BasicBlock::Create(*TheContext, "next", TheFunction);
    
    Builder->CreateCondBr(CondV, BodyBB, nextBB);

    Builder->SetInsertPoint(BodyBB);

    Value *BodyV = Body->codegen();
    if(!BodyV)
        return nullptr;

    Builder->CreateBr(CondBB);

    Builder->SetInsertPoint(nextBB);

    return ConstantInt::get(*TheContext, APInt(64, 0, true));
}

Value *ParenStmts::codegen() {
    for(size_t i = 0, e = statements.size(); i != e; ++i) {
        Value *Val = statements[i]->codegen();
        if(!Val)
            return nullptr;
    }
    
    return ConstantInt::get(*TheContext, APInt(64, 0, true));
}

Value *RetStmt::codegen() {
    Value *Val = expr->codegen();
    if(!Val)
        return nullptr;

    Builder->CreateRet(Val);

    return ConstantInt::get(*TheContext, APInt(64, 0, true));
}

Value *ParenExpr::codegen() {
    Value *exprV = expression->codegen();
    if(!exprV)
        return nullptr;

    return exprV;
}

Value *AssignExpr::codegen() {
    AllocaInst *var = NamedValues[LHS->getName()];

    Value *rhs = RHS->codegen();
    if(!rhs)
        return nullptr;
    
    Builder->CreateStore(rhs, var);

    return rhs;
}

Value *BoolExpr::codegen() {
    Value *lhs = LHS->codegen();
    Value *rhs = RHS->codegen();

    if(!lhs || !rhs)
        return nullptr;

    Value *val;
    if(Op == "<")
        val = Builder->CreateICmpSLT(lhs, rhs, "booltmp");
    else if(Op == "<=")
        val = Builder->CreateICmpSLE(lhs, rhs, "booltmp");
    else if(Op == ">")
        val = Builder->CreateICmpSGT(lhs, rhs, "booltmp");
    else if(Op == ">=")
        val = Builder->CreateICmpSGE(lhs, rhs, "booltmp");
    else if(Op == "==")
        val = Builder->CreateICmpEQ(lhs, rhs, "booltmp");
    else if(Op == "!=")
        val = Builder->CreateICmpNE(lhs, rhs, "booltmp");
    else
        return LogGenError("invalid binary (bool) operator " + Op);

    Value *V = Builder->CreateIntCast(val, Type::getInt64Ty(*TheContext), false, "restmp");

    return val;
}

Value *IntExpr::codegen() {
    Value *lhs = LHS->codegen();
    Value *rhs = RHS->codegen();

    if(!lhs || !rhs)
        return nullptr;
    
    if(Op == "+")
        return Builder->CreateAdd(lhs, rhs, "inttmp");
    else if(Op == "-")
        return Builder->CreateSub(lhs, rhs, "inttmp");
    else
        return LogGenError("invalid binary (int) operator " + Op);
}

Value *TermExpr::codegen() {
    Value *lhs = LHS->codegen();
    Value *rhs = RHS->codegen();

    if(!lhs || !rhs)
        return nullptr;
    
    if(Op == "*")
        return Builder->CreateMul(lhs, rhs, "termtmp");
    else if(Op == "/")
        return Builder->CreateSDiv(lhs, rhs, "termtmp");
    else
        return LogGenError("invalid binary (term) operator " + Op);
}

Value *HighLevelExpr::codegen() {
    Value *exprV = expr->codegen();
    if(!exprV)
        return nullptr;

    return exprV;
}


// * Main
void CreatePrintF() {
    // * Create Print function
    Type *intType = Type::getInt64Ty(*TheContext);
    vector<Type *> printfArgsTypes({Type::getInt8Ty(*TheContext)});
    FunctionType *printfType = FunctionType::get(intType, printfArgsTypes, true);
    Function *prFunc = Function::Create(printfType, Function::ExternalLinkage, "printf", TheModule.get());
    
    CurrScope->add("printf");

    vector<Type *> printArgsTypes(1, Type::getInt64Ty(*TheContext));
    FunctionType *printType = FunctionType::get(Type::getInt64Ty(*TheContext), printArgsTypes, false);
    Function *printFunc = Function::Create(printType, Function::ExternalLinkage, "print", TheModule.get());
    
    size_t Idx = 0;
    for (auto &Arg : printFunc->args())
        Arg.setName("outputValue");
    
    CurrScope->add("print");

    BasicBlock *enter = BasicBlock::Create(*TheContext, "entry", printFunc);
    Builder->SetInsertPoint(enter);

    map<string, AllocaInst *> OldBinds;
    for (auto &Arg : printFunc->args()) {
        string ArgName = string(Arg.getName());

        if(NamedValues.count(ArgName))
            OldBinds[ArgName] = NamedValues[ArgName];

        AllocaInst *ArgAlloca = Builder->CreateAlloca(Type::getInt64Ty(*TheContext), nullptr, ArgName);

        Builder->CreateStore(&Arg, ArgAlloca);

        NamedValues[ArgName] = ArgAlloca;
    }

    Value *out_str = Builder->CreateGlobalStringPtr("Output: %lli \n", "output");

    vector<Value *> argsV({out_str});
    for(auto &Arg : printFunc->args()) {
        argsV.push_back(&Arg);
    }

    Builder->CreateCall(prFunc, argsV, "calltmp");
    Builder->CreateRet(ConstantInt::get(*TheContext, APInt(64, 0, true)));

    for(auto [N, Val] : OldBinds) 
        NamedValues[N] = Val;
}

int GenerateObjFile(string Filename) {
    // * GENERATE OBJ FILE
    // Initialize the target registry etc.
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    auto TargetTriple = sys::getDefaultTargetTriple();
    TheModule->setTargetTriple(TargetTriple);

    std::string Error;
    auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

    // Print an error and exit if we couldn't find the requested target.
    // This generally occurs if we've forgotten to initialise the
    // TargetRegistry or we have a bogus target triple.
    if (!Target) {
        errs() << Error;
        return 1;
    }

    auto CPU = "generic";
    auto Features = "";

    TargetOptions opt;
    auto TheTargetMachine = Target->createTargetMachine(
        TargetTriple, CPU, Features, opt, Reloc::PIC_);

    TheModule->setDataLayout(TheTargetMachine->createDataLayout());

    std::error_code EC;
    raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

    if (EC) {
        errs() << "Could not open file: " << EC.message();
        return 1;
    }

    legacy::PassManager pass;
    auto FileType = CodeGenFileType::ObjectFile;

    if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
        errs() << "TheTargetMachine can't emit a file of this type";
        return 1;
    }

    pass.run(*TheModule);
    dest.flush();

    outs() << "Wrote " << Filename << "\n";

    return 0;
}

int main(int argc, char *argv[]) {
    if(argc == 1)
        return 0;

    // * File init
    ifstream file(argv[1]);
    std::stringstream ss;
    ss << file.rdbuf();
    file.close();

    compileFile = ss.str();
    file_size = compileFile.size();

    CurrTok = getNextToken();
    
    // * Initialize LLVM Variables
    InitializeModule();

    // * Print func create
    CreatePrintF();

    // * Main func create
    FunctionType *FT = FunctionType::get(Type::getInt64Ty(*TheContext), false);
    Function *main_fn = Function::Create(FT, Function::ExternalLinkage, "main", TheModule.get());

    BasicBlock *entryBB = BasicBlock::Create(*TheContext, "entry", main_fn);
    Builder->SetInsertPoint(entryBB);

   
    // * RUN Parse and Codegen
    unique_ptr<InputAST> inp = Input();

    if(!inp)
        return 1;
    
    Value *inputV = inp->codegen();

    if(!inputV)
        return 1;

    Builder->CreateRet(ConstantInt::get(*TheContext, APInt(64, 0, true)));

    cout << "Completly parsing\n"; 

    // * output llvm ir
    // TheModule->print(errs(), nullptr);
    
    // * generate obj file
    string filename = argv[1];
    size_t i, n = filename.size();
    for(i = n-1; filename[i] != '/'; --i);
    filename = filename.substr(i+1, n-i-4) + 'o';

    return GenerateObjFile(filename);
}
