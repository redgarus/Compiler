// Scope

class Scope {
    string scopeName;
    unordered_set<string> scopeNames{};
    shared_ptr<Scope> Prev;

public:
    bool find(const string& name) const {
        if(scopeNames.count(name) == 1)
            return true;
        else if(scopeName != "global")
            return Prev->find(name);
        else 
            return scopeNames.count(name) == 1;
        
    }

    void add(const string& name) {
        scopeNames.insert(name);
    }

    void remove(const string& name) {
        scopeNames.erase(name);
    }

    const string getName() const {
        return scopeName;
    }

    shared_ptr<Scope> getPrev() {
        return Prev;
    }

    Scope(const string& name,
        shared_ptr<Scope> prev_scope) : scopeName(name), Prev(std::move(prev_scope)) {}
};

// Expressions

class ExprAST {
public:
    bool isLvalue = false;
    string name = "miss";

    virtual ~ExprAST() = default;
    virtual Value *codegen() = 0;

    virtual const string& getName() const {
        return name;
    };
};

class AssignExpr: public ExprAST {
    unique_ptr<ExprAST> LHS, RHS;
public:
    AssignExpr(unique_ptr<ExprAST> LHS,
                unique_ptr<ExprAST> RHS) : LHS(std::move(LHS)), RHS(std::move(RHS)) {}
    
    Value *codegen() override;
};

class BoolExpr: public ExprAST {
    string Op;
    unique_ptr<ExprAST> LHS, RHS;

public:
    BoolExpr(const string& Op,
        unique_ptr<ExprAST> LHS, 
        unique_ptr<ExprAST> RHS) : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

    Value *codegen() override;
    
};

class IntExpr: public ExprAST {
    string Op;
    unique_ptr<ExprAST> LHS, RHS;
    
public:
    IntExpr(const string& Op,
        unique_ptr<ExprAST> LHS, 
        unique_ptr<ExprAST> RHS) : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
    Value *codegen() override;

    
};

class TermExpr: public ExprAST {
    string Op;
    unique_ptr<ExprAST> LHS, RHS;

public:
    TermExpr(const string& Op,
        unique_ptr<ExprAST> LHS,
        unique_ptr<ExprAST> RHS) : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

    Value *codegen() override;
};

class ParenExpr: public ExprAST {
    unique_ptr<ExprAST> expression;
public:
    ParenExpr(unique_ptr<ExprAST> expr) : expression(std::move(expr)) {}
    Value *codegen() override;
};

class IDExpr: public ExprAST {
    string name;
public:
    const string& getName() const override { return name; }
    
    IDExpr(const string& name) : name(name) { isLvalue = true; }
    Value *codegen() override;
};

class CallExpr: public ExprAST {
    string name;
    vector<unique_ptr<ExprAST>> args;
public:
    CallExpr(const string& name,
            vector<unique_ptr<ExprAST>> args) : name(name), args(std::move(args)) {}
    Value *codegen() override;
};

class NumberExpr: public ExprAST {
    ll ivalue;
public:
    NumberExpr(ll ivalue) : ivalue(ivalue) {}
    Value *codegen() override;
};

// Statements

class StmtAST {
public:
    virtual ~StmtAST() = default;
    virtual Value *codegen() = 0;
};

class HighLevelExpr: public StmtAST {
    unique_ptr<ExprAST> expr;
public:
    HighLevelExpr(unique_ptr<ExprAST> expr) : expr(std::move(expr)) {}
    Value *codegen() override;

};

class FuncStmt: public StmtAST {
    string name;
    vector<string> args;
    unique_ptr<StmtAST> body;

public:
    FuncStmt(const string& name, 
            vector<string> args,
            unique_ptr<StmtAST> body) : name(name), args(std::move(args)), body(std::move(body)) {}
    Function *codegen() override;
    
};

class RetStmt: public StmtAST {
    unique_ptr<ExprAST> expr;
public:
    RetStmt(unique_ptr<ExprAST> expr) : expr(std::move(expr)) {}
    Value *codegen() override;

};

class ParenStmts: public StmtAST {
    vector<unique_ptr<StmtAST>> statements;
public:
    ParenStmts(vector<unique_ptr<StmtAST>> stmts) : statements(std::move(stmts)) {}
    Value *codegen() override;

};

class VarStmt: public StmtAST {
    string VarName;
    unique_ptr<ExprAST> Val;
public:
    VarStmt(
        const string& VarName,
        unique_ptr<ExprAST> Val) : VarName(VarName), Val(std::move(Val)) {}
    Value *codegen() override;
};

class IfStmt: public StmtAST {
    unique_ptr<ExprAST> Cond;
    unique_ptr<StmtAST> Body;
public:
    
    IfStmt(unique_ptr<ExprAST> cond,
           unique_ptr<StmtAST> body): Cond(std::move(cond)), Body(std::move(body)) {}
    Value *codegen() override;
};

class WhileStmt: public StmtAST {
    unique_ptr<ExprAST> Cond;
    unique_ptr<StmtAST> Body;
public:
    
    WhileStmt(unique_ptr<ExprAST> cond,
           unique_ptr<StmtAST> body): Cond(std::move(cond)), Body(std::move(body)) {}
    Value *codegen() override;
};

// Input

class InputAST {
    vector<unique_ptr<StmtAST>> statements;
public:
    InputAST(vector<unique_ptr<StmtAST>> stmts) : statements(std::move(stmts)) {}
    Value *codegen();
};