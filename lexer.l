%{
    #include <stdio.h>
    #include "parser.tab.h"
%}

%option noyywrap

%%
\$\$[^\$]*\$\$ { return COMMENT; }

void|bool|char|int|float|string { yylval.string = strdup(yytext); return TYPE; }

[-]?([1-9][0-9]*)|0 { return INTCONSTANT; }
([-]?)(([1-9][0-9]*)|0)\.[0-9]* { return FLOATCONSTANT; }
\'.\' { return CHARCONSTANT; }
\"([^"])*\" { return STRINGCONSTANT; }
true|false { return BOOLCONSTANT; }

"main" { yylval.string = strdup(yytext); return MAIN; }

"return" { return RETURN; }
"class" { return CLASS; }

"constructor" { return CONSTRUCTOR; }
"destructor" { return DESTRUCTOR; }

"if" { yylval.string = strdup(yytext); return IF; }
"else" { yylval.string = strdup(yytext); return ELSE; }
"while" { yylval.string = strdup(yytext); return WHILE; }
"for" { yylval.string = strdup(yytext); return FOR; }

">" { return GT; }
">=" { return GEQ; }
"==" { return EQ; }
"!=" { return NEQ; }
"<" { return LT; }
"<=" { return LEQ; }

"&&" { return AND; }
"||" { return OR; }
"!"  { return NOT; }

Print { return PRINT; }
TypeOf { return TYPEOF; }

[_a-zA-Z][_a-zA-Z0-9]* { yylval.string = strdup(yytext); return ID; }

[ \t] { ; }
\n { yylineno ++; }
. { return yytext[0]; }
%%