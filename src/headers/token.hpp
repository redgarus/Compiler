enum class TOKEN
{
    INTEGER,
    IDENTIFIER,
    IF, WHILE, VAR, DO, FUNC, RETURN,
    NONETYPE, WAIT,
    LBRA, RBRA, LBAR, RBAR, LBRACE, RBRACE, SEMICOL, COMMA,
    PLUS, MINUS, DIV, MUL,
    NOPE, ASSIGN, LS, GT, EQ, NOEQ, GTEQ, LSEQ,
    SYMBOL, UNDEFINED, EOFILE, STRING
};

unordered_map<string, TOKEN> tokenTable {
    {"if", TOKEN::IF}, {"while", TOKEN::WHILE}, 
    {"var", TOKEN::VAR},  {"func", TOKEN::FUNC}, 
    {"do", TOKEN::DO},
    {"return", TOKEN::RETURN},
    {"{", TOKEN::LBRA}, {"}", TOKEN::RBRA},
    {"(", TOKEN::LBAR}, {")", TOKEN::RBAR},
    {"[", TOKEN::LBRACE}, {"]", TOKEN::RBRACE},
    {";", TOKEN::SEMICOL}, {",", TOKEN::COMMA},
    {"+", TOKEN::PLUS}, {"-", TOKEN::MINUS}, {"/", TOKEN::DIV}, {"*", TOKEN::MUL},
    {"!", TOKEN::NOPE},  {"=", TOKEN::ASSIGN}, {"<", TOKEN::LS}, {">", TOKEN::GT},
    {"!=", TOKEN::NOEQ},  {"==", TOKEN::EQ}, {"<=", TOKEN::LSEQ}, {">=", TOKEN::GTEQ}
};

std::ostream& operator << (std::ostream& out, const TOKEN& t) {
    switch(t) {
        case TOKEN::IF: return (out << "IF");
        case TOKEN::FUNC: return (out << "FUNC");
        case TOKEN::RETURN: return (out << "RETURN");
        case TOKEN::DO: return (out << "DO");
        case TOKEN::IDENTIFIER: return (out << "ID");
        case TOKEN::VAR: return (out << "VAR");
        case TOKEN::WHILE: return (out << "WHILE");
        case TOKEN::INTEGER: return (out << "INTEGER");

        case TOKEN::COMMA: return (out << "COMMA");
        case TOKEN::LBRA: return (out << "LBRA");
        case TOKEN::RBRA: return (out << "RBRA");
        case TOKEN::LBAR: return (out << "LBAR");
        case TOKEN::RBAR: return (out << "RBAR");
        case TOKEN::LS: return (out << "LESSER");
        case TOKEN::GT: return (out << "GREATER");
        case TOKEN::LBRACE: return (out << "LBRACE");
        case TOKEN::RBRACE: return (out << "RBRACE");
        case TOKEN::PLUS: return (out << "PLUS");
        case TOKEN::MINUS: return (out << "MINUS");
        case TOKEN::DIV: return (out << "DIV");
        case TOKEN::MUL: return (out << "MUL");
        case TOKEN::NOPE: return (out << "NO");
        case TOKEN::EQ: return (out << "EQUAL");
        case TOKEN::NOEQ: return (out << "NON EQUAL");
        case TOKEN::LSEQ: return (out << "LESS OR EQUAL");
        case TOKEN::GTEQ: return (out << "GREAT OR EQUAL");
        case TOKEN::ASSIGN: return (out << "ASSIGN");
        case TOKEN::SEMICOL: return (out << "SEMICOL");

        case TOKEN::EOFILE: return (out << "EOF");

        default: return (out << "UNDEFINED");
    }
}