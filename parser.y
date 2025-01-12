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
    #define ERRMSG(format, ...) { printf(format, ##__VA_ARGS__); exit(1); }
    
    class SymTable* currentScope;
%}

%union {
    char *string;

    struct {
        const char* strings[256];
        int lg;
    } stringArray;

    struct type {
        char *string;
        int size;
    } type;
    
    struct parameter {
        char* name;
        struct type type;
    } parameter;

    struct {
        struct parameter params[256];
        int lg; 
    } parameterArray;

    struct expression {
        char* type;
        char* value;
    } expression;

    struct {
        struct expression exp[256];
        int lg;
    } expressionArray;
}

%token COMMENT RETURN CLASS PRINT TYPEOF GT GEQ EQ NEQ LEQ LT

%token <string> IF ELSE FOR WHILE ID TYPE MAIN CONSTRUCTOR DESTRUCTOR
%token <string> STRINGCONSTANT CHARCONSTANT INTCONSTANT FLOATCONSTANT BOOLCONSTANT

%start Root

%left OR
%left AND
%left GT GEQ EQ NEQ LEQ LT
%left '+' '-'
%left '*' '/'
%left '^'
%right NOT
%left COMMENT

%type <type> Type 
%type <stringArray> IdList
%type <parameter> Parameter
%type <parameterArray> ParametersList
%type <expression> Expression BooleanExpression ArithmeticExpression Term BooleanTerm FunctionCall Identifier
%type <expressionArray> ExpressionList

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
                class vector<string> params;
                for(int i = 0; i < $3.lg; i ++) {
                    params.push_back($3.params[i].name);
                    int declarationLine = functionScope->InsertVariable(yylineno, $3.params[i].name, $3.params[i].type.string, $3.params[i].type.size);
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
                        int declarationLine = currentScope->InsertVariable(yylineno, $1.strings[i], $3, 1);
                        if(declarationLine != 0) ERRMSG("line %d: Variable '%s' already defined at line %d\n", yylineno, $1.strings[i], declarationLine);
                    }
                }
                | IdList ':' Type { 
                    for(int i = 0; i < $1.lg; i ++) {
                        int declarationLine = currentScope->InsertVariable(yylineno, $1.strings[i], $3.string, $3.size);
                        if(declarationLine != 0) ERRMSG("line %d: Variable '%s' already defined at line %d\n", yylineno, $1.strings[i], declarationLine);
                    }
                }
                ;

IdList: IdList ',' ID { $$ = $1; $$.strings[$1.lg] = $3; $$.lg++; }
      | ID { $$.strings[0] = $1; $$.lg = 1; }
      ;

FunctionSection: FunctionDefinition FunctionSection
               | COMMENT FunctionSection
               | ';'
               ;

FunctionDefinition: ID '(' ParametersList ')' ':' TYPE {
                        class SymTable* functionScope = new SymTable($1, currentScope);
                        class vector<string> params;
                        for(int i = 0; i < $3.lg; i ++) {
                            params.push_back($3.params[i].name);
                            int declarationLine = functionScope->InsertVariable(yylineno, $3.params[i].name, $3.params[i].type.string, $3.params[i].type.size);
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
         | Identifier '=' Expression {
            if(strcmp($1.type, $3.type) != 0)
                ERRMSG("line %d: Identifier of different type than expression %s %s\n", yylineno, $1.type, $3.type);
         }
         ;

EntryPoint: MAIN '(' ')' {
                class SymTable* functionScope = new SymTable($1, currentScope);
                int declarationLine = currentScope->InsertFunction(yylineno, $1, $1, {}, functionScope);
                if(declarationLine != 0) ERRMSG("Main function already defined at line: %d\n", declarationLine);
                currentScope = functionScope;
            } '{' FunctionScope '}' { currentScope = currentScope->GetParentTable(); }
          ;

Expression: BooleanExpression { $$ = $1; }
          | ArithmeticExpression { $$ = $1; }
          | CHARCONSTANT { strcpy($$.type, "char"); $$.value = $1; }
          | STRINGCONSTANT { strcpy($$.type, "string"); $$.value = $1; }
          ;

ExpressionList: ExpressionList ',' Expression { $$ = $1; $$.exp[$$.lg] = $3; $$.lg ++; }
              | Expression { $$.exp[0] = $1; $$.lg = 1; }
              | %empty { $$.lg = 0; }
              ;

ArithmeticExpression: '(' ArithmeticExpression ')' { $$ = $2; }
                    | ArithmeticExpression '+' ArithmeticExpression {
                        if(strcmp($1.type, $3.type) != 0) {
                            ERRMSG("line %d: Operators of different types. Casting not supported: %s %s\n", yylineno, $1.type, $3.type);
                        }
                    }
                    | ArithmeticExpression '-' ArithmeticExpression {
                        if(strcmp($1.type, $3.type) != 0) {
                            ERRMSG("line %d: Operators of different types. Casting not supported: %s %s\n", yylineno, $1.type, $3.type);
                        }
                    }
                    | ArithmeticExpression '/' ArithmeticExpression {
                        if(strcmp($1.type, $3.type) != 0) {
                            ERRMSG("line %d: Operators of different types. Casting not supported: %s %s\n", yylineno, $1.type, $3.type);
                        }
                    }
                    | ArithmeticExpression '*' ArithmeticExpression {
                        if(strcmp($1.type, $3.type) != 0) {
                            ERRMSG("line %d: Operators of different types. Casting not supported: %s %s\n", yylineno, $1.type, $3.type);
                        }
                    }
                    | Term { $$ = $1; }
                    ;

Term: INTCONSTANT { strcpy($$.type, "int"); $$.value = $1; }
    | FLOATCONSTANT { strcpy($$.type, "float"); $$.value = $1; }
    | FunctionCall { $$ = $1; }
    | Identifier { $$ = $1; }
    ;

BooleanExpression: '(' BooleanExpression ')' { $$ = $2; }
                 | ArithmeticExpression GT ArithmeticExpression { strcpy($$.type, "bool"); }
                 | ArithmeticExpression GEQ ArithmeticExpression { strcpy($$.type, "bool"); }
                 | ArithmeticExpression EQ ArithmeticExpression { strcpy($$.type, "bool"); }
                 | ArithmeticExpression NEQ ArithmeticExpression { strcpy($$.type, "bool"); }
                 | ArithmeticExpression LEQ ArithmeticExpression { strcpy($$.type, "bool"); }
                 | ArithmeticExpression LT ArithmeticExpression { strcpy($$.type, "bool"); }
                 | BooleanExpression EQ BooleanExpression { strcpy($$.type, "bool"); }
                 | BooleanExpression NEQ BooleanExpression { strcpy($$.type, "bool"); }
                 | BooleanExpression AND BooleanExpression { strcpy($$.type, "bool"); }
                 | BooleanExpression OR BooleanExpression { strcpy($$.type, "bool"); }
                 | NOT BooleanExpression { strcpy($$.type, "bool"); }
                 | BooleanTerm { $$ = $1; }
                 ;

BooleanTerm: BOOLCONSTANT { strcpy($$.type, "bool"); $$.value = $1; }
           | FunctionCall '?'{ $$ = $1; }
           | Identifier '?' { $$ = $1; }
           ;


Identifier: ID { 
                if(currentScope->IsDefined($1) == false) 
                    ERRMSG("line %d: Undefined variable %s\n", yylineno, $1)
                strcpy($$.type, currentScope->GetType($1).c_str());
            }
          | ID '[' Expression ']' {
                if(currentScope->IsDefined($1) == false) 
                    ERRMSG("line %d: Undefined variable %s\n", yylineno, $1)
                string temp = currentScope->GetType($1);
                strcpy($$.type, temp.substr(0, temp.find('[')).c_str());
            }
          | ID '.' ID {
                if(currentScope->IsDefined($1) == false) 
                    ERRMSG("line %d: Undefined variable %s\n", yylineno, $1)
                string classType = currentScope->GetType($1);
                class SymTable* classTable = currentScope->GetSymTable(classType);
                if(classTable == nullptr)
                    ERRMSG("line %d: Variable %s is not an instance of a class\n", yylineno, $1);
                if(classTable->ExistsInScope($3) == false)
                    ERRMSG("line %d: Class %s has no member %s\n", yylineno, classTable->GetPath().c_str(), $3);
                strcpy($$.type, classTable->GetType($3).c_str());
            }
          ;

ParametersList: ParametersList ',' Parameter { $$ = $1; $$.params[$$.lg] = $3; $$.lg ++; }
              | Parameter { $$.params[0] = $1; $$.lg = 1; }
              | %empty { $$.lg = 0; }
              ;

Parameter: ID ':' Type { $$.name = $1; $$.type = $3; }
         ;

Type: TYPE { $$.string = $1; $$.size = 1; }
    | TYPE '[' Expression ']' { $$.string = $1; $$.size = 10; }
    | ID '(' ExpressionList ')' { $$.string = $1; $$.size = 1; }
    ;

FunctionCall: ID '(' ExpressionList ')' {
                if(currentScope->IsDefined($1) == false)
                    ERRMSG("line %d: Undefined function %s\n", yylineno, $1)
                class SymTable* functionTable = currentScope->GetSymTable($1);
                vector<string> params = functionTable->GetParentTable()->GetParams($1);
                if($3.lg != params.size())
                    ERRMSG("line %d: Expression list does not match with required parameters\n", yylineno);
                for(int i = 0; i < $3.lg; i ++)
                    if(strcmp($3.exp[i].type, functionTable->GetType(params[i]).c_str()) != 0)
                        ERRMSG("line %d: Parameter #%d does not match the required type: %s %s\n", yylineno, i + 1, $3.exp[i].type, functionTable->GetType(params[i]).c_str());
                strcpy($$.type, currentScope->GetType($1).c_str());
            }
            | ID '.' ID '(' ExpressionList ')' {
                if(currentScope->IsDefined($1) == false)
                    ERRMSG("line %d: Undefined variable %s\n", yylineno, $1)
                string classType = currentScope->GetType($1);
                class SymTable* classTable = currentScope->GetSymTable(classType);
                if(classTable == nullptr)
                    ERRMSG("line %d: Variable %s is not an instance of a class\n", yylineno, $1);
                if(classTable->ExistsInScope($3) == false)
                    ERRMSG("line %d: Class %s has no method %s\n", yylineno, classTable->GetPath().c_str(), $3);
                vector<string> params = classTable->GetParams($3);
                if($5.lg != params.size())
                    ERRMSG("line %d: Expression list does not match with required parameters\n", yylineno);
                class SymTable* functionTable = classTable->GetSymTable($3);
                for(int i = 0; i < $5.lg; i ++)
                    if(strcmp($5.exp[i].type, functionTable->GetType(params[i]).c_str()) != 0)
                        ERRMSG("line %d: Parameter #%d does not match the required type: %s %s\n", yylineno, i + 1, $5.exp[i].type, functionTable->GetType(params[i]).c_str());
                strcpy($$.type, classTable->GetType($3).c_str());
            }
            | PRINT '(' Expression ')' { strcpy($$.type, "void"); cout << "idk how to print yet\n"; }
            | TYPEOF '(' Expression ')' { strcpy($$.type, "void"); printf("%s\n", $3.type); }
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