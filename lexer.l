%{
    #include <stdio.h>
    #include "parser.tab.h"
%}

%option noyywrap

%%
\$\$[^\$]*\$\$ { return COMMENT; }

void|bool|char|int|float|string { yylval.string = strdup(yytext); return TYPE; }

[-]?([1-9][0-9]*)|0 { yylval.string = strdup(yytext); return INTCONSTANT; }
([-]?)(([1-9][0-9]*)|0)\.[0-9]* { yylval.string = strdup(yytext); return FLOATCONSTANT; }
\'.\' { yylval.string = strdup(yytext); return CHARCONSTANT; }
\"([^"])*\" { yylval.string = strdup(yytext); return STRINGCONSTANT; }
"true"|"false" { yylval.string = strdup(yytext); return BOOLCONSTANT; }

"main" { yylval.string = strdup(yytext); return MAIN; }

"return" { return RETURN; }
"class" { return CLASS; }

"constructor" { yylval.string = strdup(yytext); return CONSTRUCTOR; }
"destructor" { yylval.string = strdup(yytext); return DESTRUCTOR; }

"if" { yylval.string = strdup(yytext); return IF; }
"else" { yylval.string = strdup(yytext); return ELSE; }
"while" { yylval.string = strdup(yytext); return WHILE; }
"for" { yylval.string = strdup(yytext); return FOR; }

">" { yylval.string = strdup(yytext); return GT; }
">=" { yylval.string = strdup(yytext); return GEQ; }
"==" { yylval.string = strdup(yytext); return EQ; }
"!=" { yylval.string = strdup(yytext); return NEQ; }
"<" { yylval.string = strdup(yytext); return LT; }
"<=" { yylval.string = strdup(yytext); return LEQ; }

"&&" { yylval.string = strdup(yytext); return AND; }
"||" { yylval.string = strdup(yytext); return OR; }
"!"  { return NOT; }

Print { return PRINT; }
TypeOf { return TYPEOF; }

[_a-zA-Z][_a-zA-Z0-9]* { yylval.string = strdup(yytext); return ID; }

[ \t] { ; }
\n { yylineno ++; }
. { return yytext[0]; }
%%