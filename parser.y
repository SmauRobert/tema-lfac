%{
    #include <iostream>
    #include <fstream>
    #include <cstring>
    #include "SymTable.hpp"

    using namespace std;

    extern int yylex();
    extern int yylineno;
    extern FILE* yyin;
    
    void yyerror(const char *);

    int errorCount = 0;
    #define ERRMSG(format, ...) { errorCount ++; printf(format, ##__VA_ARGS__); /*exit(1);*/ }
    
    class SymTable* currentScope;
%}

%union {
    char *string;
    char character;
    int integer;
    float rational;
    bool boolean;

    struct {
        const char* strings[256];
        int lg;
    } stringArray;
    
    struct parameter {
        char* name;
        char* type;
    } parameter;

    struct {
        struct parameter params[256];
        int lg; 
    } parameterArray;
}

%token COMMENT RETURN CLASS PRINT TYPEOF GT GEQ EQ NEQ LEQ LT 

%token <string> IF ELSE FOR WHILE ID TYPE MAIN CONSTRUCTOR DESTRUCTOR

%token <string> STRINGCONSTANT 
%token <character> CHARCONSTANT
%token <integer> INTCONSTANT
%token <rational> FLOATCONSTANT
%token <boolean> BOOLCONSTANT

%start Root

%left OR
%left AND
%left GT GEQ EQ NEQ LEQ LT
%left '+' '-'
%left '*' '/'
%left '^'
%right NOT
%left COMMENT

%type <string> Type Expression Term 
%type <stringArray> IdList ExpressionList
%type <parameter> Parameter
%type <parameterArray> ParametersList

%%
Root: ClassSection GlobalSection FunctionSection EntryPoint { 
        if(errorCount == 1) cout << "1 error found\n";
        else cout << errorCount << " errors found\n"; 
    }
    ;

ClassSection: ClassDefinition ClassSection
            | COMMENT ClassSection
            | ';'
            ;

ClassDefinition: CLASS ID {
                    class SymTable* classScope = new SymTable($2, currentScope);
                    int declarationLine = currentScope->InsertClass(yylineno, $2, classScope);
                    if(declarationLine != 0) ERRMSG("line %d: Class '%s' already defined at line %d\n", yylineno, $2, declarationLine);

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

Constructor: CONSTRUCTOR '(' ParametersList ')' {
                class SymTable* functionScope = new SymTable($1, currentScope);
                class vector<Symbol> params;
                for(int i = 0; i < $3.lg; i ++) {
                    params.push_back({$3.params[i].name, functionScope, {}});
                    int declarationLine = functionScope->InsertVariable(yylineno, $3.params[i].name, $3.params[i].type);
                    if(declarationLine != 0) ERRMSG("line %d: Parameter '%s' already defined\n", yylineno, $3.params[i].name);
                }
                int declarationLine = currentScope->InsertFunction(yylineno, $1, "void", params, functionScope);
                if(declarationLine != 0) ERRMSG("line %d: Function '%s' already defined at line %d\n", yylineno, $1, declarationLine);
                currentScope = functionScope;
            } '{' FunctionScope '}' { currentScope = currentScope->GetParentTable(); }
           ;

Destructor: DESTRUCTOR '(' ')' {
                class SymTable* functionScope = new SymTable($1, currentScope);
                int declarationLine = currentScope->InsertFunction(yylineno, $1, "void", {}, functionScope);
                if(declarationLine != 0) ERRMSG("line %d: Function '%s' already defined at line %d\n", yylineno, $1, declarationLine);

                currentScope = functionScope;
            } '{' FunctionScope '}' { currentScope = currentScope->GetParentTable(); }
          ;

GlobalSection: DeclareStatement ';' GlobalSection
             | COMMENT GlobalSection
             | ';'
             ;

DeclareStatement: IdList ':' TYPE '=' Expression { 
                    for(int i = 0; i < $1.lg; i ++) {
                        int declarationLine = currentScope->InsertVariable(yylineno, $1.strings[i], $3);
                        if(declarationLine != 0) ERRMSG("line %d: Variable '%s' already defined at line %d\n", yylineno, $1.strings[i], declarationLine);
                    }
                }
                | IdList ':' Type { 
                    for(int i = 0; i < $1.lg; i ++) {
                        int declarationLine = currentScope->InsertVariable(yylineno, $1.strings[i], $3);
                        if(declarationLine != 0) ERRMSG("line %d: Variable '%s' already defined at line %d\n", yylineno, $1.strings[i], declarationLine);
                    }
                }
                ;

FunctionSection: FunctionDefinition FunctionSection
               | COMMENT FunctionSection
               | ';'
               ;

FunctionDefinition: ID '(' ParametersList ')' ':' Type {
                        class SymTable* functionScope = new SymTable($1, currentScope);
                        class vector<Symbol> params;
                        for(int i = 0; i < $3.lg; i ++) {
                            params.push_back({$3.params[i].name, functionScope, {}});
                            int declarationLine = functionScope->InsertVariable(yylineno, $3.params[i].name, $3.params[i].type);
                            if(declarationLine != 0) ERRMSG("line %d: Parameter '%s' already defined\n", yylineno, $3.params[i].name);
                        }
                        int declarationLine = currentScope->InsertFunction(yylineno, $1, $6, params, functionScope);
                        if(declarationLine != 0) ERRMSG("line %d: Function '%s' already defined at line %d\n", yylineno, $1, declarationLine);
               
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
                int declarationLine = currentScope->InsertBlock(declarationLine, name, elseScope);
                if(declarationLine != 0) ERRMSG("Block already exists. How?\n");
                currentScope = elseScope; 
            } '{' BlockScope '}' { currentScope = currentScope->GetParentTable(); }
           ;

IfBlock: {
            string name = "if(" + to_string(yylineno) + ')';
            class SymTable* ifScope = new SymTable(name, currentScope);
            int declarationLine = currentScope->InsertBlock(declarationLine, name, ifScope);
                if(declarationLine != 0) ERRMSG("Block already exists. How?\n");
            currentScope = ifScope; 
        } '{' BlockScope '}' { currentScope = currentScope->GetParentTable(); }
       ;

ForStatement: FOR {
                string name = string($1) + '(' + to_string(yylineno) + ')';
                class SymTable* forScope = new SymTable(name, currentScope);
                int declarationLine = currentScope->InsertBlock(declarationLine, name, forScope);
                if(declarationLine != 0) ERRMSG("Block already exists. How?\n");
                currentScope = forScope; 
            } '(' Statement ';' BooleanExpression ';' Statement ')' '{' BlockScope '}' { currentScope = currentScope->GetParentTable(); }
            ;

WhileStatement: WHILE '(' BooleanExpression ')' {
                    string name = string($1) + '(' + to_string(yylineno) + ')';
                    class SymTable* whileScope = new SymTable(name, currentScope);
                    int declarationLine = currentScope->InsertBlock(declarationLine, name, whileScope);
                if(declarationLine != 0) ERRMSG("Block already exists. How?\n");
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
                int declarationLine = currentScope->InsertFunction(yylineno, $1, $1, {}, functionScope);
                if(declarationLine != 0) ERRMSG("Main function already defined at line: %d\n", declarationLine);
                currentScope = functionScope;
            } '{' FunctionScope '}' { currentScope = currentScope->GetParentTable(); }
          ;

IdList: IdList ',' ID { $$ = $1; $$.strings[$1.lg] = $3; $$.lg++; }
      | ID { $$.strings[0] = $1; $$.lg = 1; }
      ;


Expression: BooleanExpression { }
          | ArithmeticExpression {  }
          | CHARCONSTANT { $$ = "char"; }
          | STRINGCONSTANT { $$ = "string"; }
          ;

ExpressionList: ExpressionList ',' Expression { $$ = $1; $$.strings[$$.lg] = $3; $$.lg ++; }
              | Expression { $$.strings[0] = $1; $$.lg = 1; }
              | %empty { $$.lg = 0; }
              ;

ArithmeticExpression: '(' ArithmeticExpression ')'
                    | ArithmeticExpression '+' ArithmeticExpression
                    | ArithmeticExpression '-' ArithmeticExpression
                    | ArithmeticExpression '/' ArithmeticExpression
                    | ArithmeticExpression '*' ArithmeticExpression
                    | ArithmeticExpression '^' ArithmeticExpression
                    | Term
                    ;

Term: INTCONSTANT { $$ = "int"; }
    | FLOATCONSTANT { $$ = "float"; }
    | FunctionCall { }
    | Identifier {}
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


Identifier: ID { if(currentScope->IsVariableDefined($1) == false) ERRMSG("line %d: Undefined variable %s\n", yylineno, $1)}
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
    | Type '[' Expression ']' { $$ = strcat($1, "[]"); }
    | ID '(' ExpressionList ')'
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

void yyerror(const char * msg) {
    printf("[ error @ line %d ] %s\n", yylineno, msg);
    exit(1);
}

int main(int argc, char **argv) {
    yyin = fopen(argv[1], "r");
    currentScope = new SymTable("~");
    yyparse();
    ofstream fout("SymbolTable.output");
    fout << currentScope->String();
}