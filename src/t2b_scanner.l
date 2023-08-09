%{
    #include <t2b/ast.hpp>
    #include <t2b_parser.hpp>
    extern int yylex();
%}
%option noyywrap
%%
[ \t\r\n]+              ;
[0-9]+                { return T_UINT; }
-[0-9]+                { return T_INT; }
-?[0-9]+\.[0-9]+        { return T_FLOAT; }
[A-Za-z_][A-Za-z0-9_]*  { return T_ID; }
%%