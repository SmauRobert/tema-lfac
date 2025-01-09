%{
    #include <iostream>
    #include "SymTable.hpp"

    using namespace std;

    extern int yylex();
    extern int yylineno;
    extern FILE* yyin;
    
    void yyerror(char *);

    int errorCount = 0;
    class SymTable* currentScope;
%}

%union {
    char *string;
    struct {
        const char* strings[256];
        int lg;
    } stringArray;
    
    struct parameter {
        const char* name;
        const char* type;
    } parameter;

    struct {
        struct parameter params[256];
        int lg; 
    } parameterArray;
}

%token COMMENT RETURN CLASS PRINT TYPEOF GT GEQ EQ NEQ LEQ LT CONSTRUCTOR DESTRUCTOR
%token INTCONSTANT FLOATCONSTANT BOOLCONSTANT STRINGCONSTANT CHARCONSTANT

%token <string> IF ELSE FOR WHILE ID TYPE MAIN

%start Root

%left OR
%left AND
%left GT GEQ EQ NEQ LEQ LT
%left '+' '-'
%left '*' '/'
%left '^'
%right NOT
%left COMMENT

%type<string> Type
%type<stringArray> IdList
%type<parameter> Parameter
%type<parameterArray> ParametersList

%%

Root: ClassSection GlobalSection FunctionSection EntryPoint { if(errorCount == 0) cout << "Compiled successfully!\n"; }
    ;

ClassSection: ClassDefinition ClassSection
            | COMMENT ClassSection
            | ';'
            ;

ClassDefinition: CLASS ID {
                    class SymTable* classScope = new SymTable($2, currentScope);
                    if(currentScope->InsertClass(yylineno, $2, classScope) == false)
                        yyerror("Class already exists");
                    currentScope = classScope;
                } '{' ClassScope '}' { currentScope = currentScope->GetParentTable(); }
               ;

ClassScope: ClassScope DeclareStatement ';'
          | ClassScope COMMENT
          | ClassScope FunctionDefinition
          | ClassScope Constructor
          | ClassScope Destructor
          | %empty
          ;

Constructor: CONSTRUCTOR '(' ParametersList ')' '{' FunctionScope '}'
           ;

Destructor: DESTRUCTOR '(' ')' '{' FunctionScope '}' 
          ;

GlobalSection: DeclareStatement ';' GlobalSection
             | COMMENT GlobalSection
             | ';'
             ;

DeclareStatement: IdList ':' TYPE '=' Expression { 
                    for(int i = 0; i < $1.lg; i ++)
                        if(currentScope->InsertVariable(yylineno, $1.strings[i], $3) == false)
                            yyerror("Identifier already exists");
                }
                | IdList ':' Type { 
                    for(int i = 0; i < $1.lg; i ++)
                        if(currentScope->InsertVariable(yylineno, $1.strings[i], $3) == false)
                            yyerror("Identifier already exists");
                }
                ;

FunctionSection: FunctionDefinition FunctionSection
               | COMMENT FunctionSection
               | ';'
               ;

FunctionDefinition: ID '(' ParametersList ')' ':' Type {
                        class SymTable* functionScope = new SymTable($1, currentScope);
                        class vector<Symbol> params;
                        for(int i = 0; i < $3.lg; i ++)
                            params.push_back({$3.params[i].name, $3.params[i].type, {}});
                        if(currentScope->InsertFunction(yylineno, $1, $6, params, functionScope) == false)
                            yyerror("Function already exists");
                        currentScope = functionScope;
                    } '{' FunctionScope '}' { currentScope = currentScope->GetParentTable(); }
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

IfStatement: IF '(' BooleanExpression ')' IfBlock
           | IF '(' BooleanExpression ')' IfBlock ELSE IfStatement
           | IF '(' BooleanExpression ')' IfBlock ELSE {
                string name = "else(" + to_string(yylineno) + ')';
                class SymTable* elseScope = new SymTable(name, currentScope);
                if(currentScope->InsertBlock(yylineno, name, elseScope) == false)
                    yyerror("Block already exists (how?!)");
                currentScope = elseScope; 
            } '{' BlockScope '}' { currentScope = currentScope->GetParentTable(); }
           ;

IfBlock: {
                string name = "if(" + to_string(yylineno) + ')';
                class SymTable* ifScope = new SymTable(name, currentScope);
                if(currentScope->InsertBlock(yylineno, name, ifScope) == false)
                    yyerror("Block already exists (how?!)");
                currentScope = ifScope; 
            } '{' BlockScope '}' { currentScope = currentScope->GetParentTable(); }
       ;

ForStatement: FOR {
                string name = string($1) + '(' + to_string(yylineno) + ')';
                class SymTable* forScope = new SymTable(name, currentScope);
                if(currentScope->InsertBlock(yylineno, name, forScope) == false)
                    yyerror("Block already exists (how?!)");
                currentScope = forScope; 
            } '(' Statement ';' BooleanExpression ';' Statement ')' '{' BlockScope '}' { currentScope = currentScope->GetParentTable(); }
            ;

WhileStatement: WHILE '(' BooleanExpression ')' {
                    string name = string($1) + '(' + to_string(yylineno) + ')';
                    class SymTable* whileScope = new SymTable(name, currentScope);
                    if(currentScope->InsertBlock(yylineno, name, whileScope) == false)
                        yyerror("Block already exists (how?!)");
                    currentScope = whileScope;
                } '{' BlockScope '}' { currentScope = currentScope->GetParentTable(); }
              ;

BlockScope: FunctionScope
          ;

Statement: DeclareStatement
         | Identifier '=' Expression
         ;

EntryPoint: MAIN '(' ')' {
                class SymTable* functionScope = new SymTable($1, currentScope);
                currentScope->InsertFunction(yylineno, $1, $1, {}, functionScope);
                currentScope = functionScope;
            } '{' FunctionScope '}' { currentScope = currentScope->GetParentTable(); }
          ;

IdList: IdList ',' ID { $$ = $1; $$.strings[$1.lg] = $3; $$.lg++; }
      | ID { $$.strings[0] = $1; $$.lg = 1; }
      ;


Expression: BooleanExpression
          | ArithmeticExpression
          | CHARCONSTANT
          | STRINGCONSTANT
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

ParametersList: ParametersList ',' Parameter { $$ = $1; $$.params[$$.lg] = $3; $$.lg ++; }
              | Parameter { $$.params[0] = $1; $$.lg = 1; }
              | %empty { $$.lg = 0; }
              ;

Parameter: ID ':' Type { $$.name = $1; $$.type = $3; }
         ;

Type: TYPE { $$ = $1; }
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
    errorCount ++;
    printf("[ error @ line %d ] %s\n", yylineno, msg);
}

int main(int argc, char **argv) {
    yyin = fopen(argv[1], "r");
    currentScope = new SymTable("~");
    yyparse();
    currentScope->Print();
}