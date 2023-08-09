%{
    #include <cstdint>
    #include <t2b/ast.hpp>
    extern int yylex();
    extern int yyparse();
    extern FILE *yyin;

    void yyerror(const char *s);
%}

%union {
    int64_t ival;
    uint64_t uval;
    double fval;
    char *sval;
}

%token <uval> T_UINT
%token <ival> T_INT
%token <fval> T_FLOAT;
%token <sval> T_ID;

%%

stub: T_UINT;

%%