%{
    #include <iostream>

    using namespace std;

    extern int yylex();
    extern int yylineno;
    extern FILE* yyin;
    
    void yyerror(char *);
%}

%union {
    char *id;
    char *type;
}

%token COMMENT MAIN RETURN CLASS IF ELSE FOR WHILE PRINT TYPEOF GT GEQ EQ NEQ LEQ LT CONSTRUCTOR DESTRUCTOR
%token INTCONSTANT FLOATCONSTANT BOOLCONSTANT STRINGCONSTANT CHARCONSTANT
%token <id>     ID
%token <type> TYPE

%start Root

%left OR
%left AND
%left GT GEQ EQ NEQ LEQ LT
%left '+' '-'
%left '*' '/'
%left '^'
%right NOT
%left COMMENT

%%

Root: ClassSection GlobalSection FunctionSection EntryPoint { cout << "Compiled successfully!\n"; }
    ;

ClassSection: ClassDefinition ClassSection
            | COMMENT ClassSection
            | ';'
            ;

ClassDefinition: CLASS ID  '{' ClassScope '}'
               ;

ClassScope: ClassScope DeclareStatement ';'
          | ClassScope COMMENT
          | ClassScope FunctionDefinition
          | ClassScope Constructor
          | ClassScope Destructor
          | %empty
          ;

Constructor: CONSTRUCTOR '(' ParametersList ')' '{' FunctionScope '}' { printf("\tClass constructor\n"); }
           ;

Destructor: DESTRUCTOR '(' ')' '{' FunctionScope '}' { printf("\tClass destructor\n"); }
          ;

GlobalSection: DeclareStatement ';' GlobalSection
             | COMMENT GlobalSection
             | ';'
             ;

DeclareStatement: IdList ':' TYPE '=' Expression
                | IdList ':' Type 
                ;

FunctionSection: FunctionDefinition FunctionSection
               | COMMENT FunctionSection
               | ';'
               ;

FunctionDefinition: ID '(' ParametersList ')' ':' Type '{' FunctionScope '}' { printf("\tDefined function %s\n", $1); }
                  ;

FunctionScope: FunctionScope Statement ';'
             | FunctionScope BlockStatement
             | FunctionScope COMMENT
             | FunctionScope ReturnStatement
             | %empty
             ;

BlockStatement: IfStatement
              | ForStatement
              | WhileStatement
              | FunctionCall ';'
              ;

IfStatement: IF '(' BooleanExpression ')' '{' BlockScope '}'
           | IF '(' BooleanExpression ')' '{' BlockScope '}' ELSE IfStatement
           | IF '(' BooleanExpression ')' '{' BlockScope '}' ELSE '{' BlockScope '}'
           ;

ForStatement: FOR '(' Statement ';' BooleanExpression ';' Statement ')' '{' BlockScope '}'
            ;

WhileStatement: WHILE '(' BooleanExpression ')' '{' BlockScope '}'
              ;

BlockScope: FunctionScope
          ;

Statement: DeclareStatement
         | Identifier '=' Expression
         ;

EntryPoint: MAIN '(' ')' '{' FunctionScope '}'
          ;

IdList: IdList ',' ID
      | ID
      ;

Expression: BooleanExpression
          | ArithmeticExpression
          ;

ArithmeticExpression: '(' ArithmeticExpression ')'
                    | ArithmeticExpression '+' ArithmeticExpression
                    | ArithmeticExpression '-' ArithmeticExpression
                    | ArithmeticExpression '/' ArithmeticExpression
                    | ArithmeticExpression '*' ArithmeticExpression
                    | ArithmeticExpression '^' ArithmeticExpression
                    | Term
                    ;

Term: INTCONSTANT
    | FLOATCONSTANT
    | CHARCONSTANT
    | STRINGCONSTANT
    | FunctionCall
    | Identifier
    ;

BooleanExpression: '(' BooleanExpression ')'
                 | ArithmeticExpression GT ArithmeticExpression
                 | ArithmeticExpression GEQ ArithmeticExpression
                 | ArithmeticExpression EQ ArithmeticExpression
                 | ArithmeticExpression NEQ ArithmeticExpression
                 | ArithmeticExpression LEQ ArithmeticExpression
                 | ArithmeticExpression LT ArithmeticExpression
                 | BooleanExpression AND BooleanExpression
                 | BooleanExpression OR BooleanExpression
                 | NOT BooleanExpression
                 | BooleanTerm
                 ;

BooleanTerm: BOOLCONSTANT
           | FunctionCall '?'
           | Identifier '?'
           ;


Identifier: ID
          | Identifier '[' Expression ']'
          | Identifier '.' ID
          ;

ParametersList: ParametersList ',' Parameter
              | Parameter
              | %empty
              ;

Parameter: ID ':' Type { printf("\tParameter %s\n", $1); }
         ;

Type: TYPE
    | Type '[' Expression ']'
    | ID '(' ExpressionList ')'
    ;

ExpressionList: ExpressionList ',' Expression
              | Expression
              | %empty
              ;

FunctionCall: ID '(' ExpressionList ')' 
            | Identifier '.' ID '(' ExpressionList ')'
            | PRINT '(' Expression ')'
            | TYPEOF '(' Expression ')'
            ;

ReturnStatement: RETURN Expression ';'
               | RETURN ';'
               ;
%%

void yyerror(char * msg) {
    printf("[ error @ line %d ] %s\n", yylineno, msg);
    exit(1);
}

int main(int argc, char **argv) {
    yyin = fopen(argv[1], "r");
    yyparse();
}