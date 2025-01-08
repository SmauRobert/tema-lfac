%{
    #include <stdio.h>
    #include "parser.tab.h"
%}

%option noyywrap

%%
\$\$[^\$]*\$\$ { printf("[ comment ] %s\n", yytext); return COMMENT; }

void|bool|char|int|float|string { printf("[ type ] %s\n", yytext); yylval.type = strdup(yytext); return TYPE; }

[-]?([1-9][0-9]*)|0 { printf("[ const int ] %s\n", yytext); return INTCONSTANT; }
([-]?)(([1-9][0-9]*)|0)\.[0-9]* { printf("[ const float ] %s\n", yytext); return FLOATCONSTANT; }
\'.\' { printf("[ const char ] %s\n", yytext); return CHARCONSTANT; }
\"([^"])*\" { printf("[ const string ] %s\n", yytext); return STRINGCONSTANT; }
true|false { printf("[ const bool ] %s\n", yytext); return BOOLCONSTANT; }

"main" { printf("[ main ]\n"); return MAIN; }

"return" { printf("[ return ]\n"); return RETURN; }
"class" { printf("[ class ]\n"); return CLASS; }

"constructor" { printf("[ constructor ]\n"); return CONSTRUCTOR; }
"destructor" { printf("[ destructor ]\n"); return DESTRUCTOR; }

"if" { printf("[ if ]\n"); return IF; }
"else" { printf("[ else ]\n"); return ELSE; }
"while" { printf("[ while ]\n"); return WHILE; }
"for" { printf("[ for ]\n"); return FOR; }

">" { printf("[ greater ]\n"); return GT; }
">=" { printf("[ greater or equal ]\n"); return GEQ; }
"==" { printf("[ equality ]\n"); return EQ; }
"!=" { printf("[ inequality ]\n"); return NEQ; }
"<" { printf("[ less ]\n"); return LT; }
"<=" { printf("[ less or equal ]\n"); return LEQ; }

"&&" { printf("[ and ]\n"); return AND; }
"||" { printf("[ or ]\n"); return OR; }
"!"  { printf("[ not ]\n"); return NOT; }

Print { printf("[ print ]\n"); return PRINT; }
TypeOf { printf("[ typeof ]\n"); return TYPEOF; }

[_a-zA-Z][_a-zA-Z0-9]* { printf("[ id ] %s\n", yytext); yylval.id = strdup(yytext); return ID; }

[ \t] { ; }
\n { yylineno ++; }
. { return yytext[0]; }
%%