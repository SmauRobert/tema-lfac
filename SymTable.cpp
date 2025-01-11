#include "SymTable.hpp"
#include <iostream>

SymTable::SymTable(string tableName, SymTable* parentTable) : parentTable(parentTable) {
    if(parentTable == nullptr) {
        tablePath = tableName;
        return;
    }

    tablePath = parentTable->GetPath() + "/" + tableName;
}

int SymTable::InsertVariable(int line, string name, string type) {
    Symbol newSymbol = { name, this, {} };
    int declarationLine = GetDeclarationLine(newSymbol);
    if(declarationLine != 0) return declarationLine;

    idData[newSymbol] = { V, type, line };

    return 0;
}

int SymTable::InsertFunction(int line, string name, string type, vector<Symbol> params, SymTable* functionTable) {
    if(functionTable == nullptr) { cout << "(tabela nula) " + functionTable->GetPath() + '\n'; return 0;}
    Symbol newSymbol = { name, this, params };
    int declarationLine = GetDeclarationLine(newSymbol);
    if(declarationLine != 0) return declarationLine;
    
    idData[newSymbol] = { F, type, line };
    if(functionTable == nullptr) { cout << "(tabela nula2) " + functionTable->GetPath() + '\n'; return 0;}
    childrenTables[newSymbol] = functionTable;
    return 0;
}

int SymTable::InsertClass(int line, string name, SymTable* classTable) {
    Symbol newSymbol = { name, this, {} };
    int declarationLine = GetDeclarationLine(newSymbol);
    if(declarationLine != 0) return declarationLine;

    idData[newSymbol] = { C, name, line }; 
    childrenTables[newSymbol] = classTable;
    return 0;
}

int SymTable::InsertBlock(int line, string name, SymTable* blockTable) {
    Symbol newSymbol = { name, this, {} };
    int declarationLine = GetDeclarationLine(newSymbol);
    if(declarationLine != 0) return declarationLine;
    
    idData[newSymbol] = { B, "void", line };
    childrenTables[newSymbol] = blockTable;
    return 0;
}

bool SymTable::IsVariableDefined(string name) {
    if(idData.find({name, this, {}}) != idData.end()) return true;
    if(parentTable == nullptr) return false;
    return parentTable->IsVariableDefined(name);
}

string SymTable::GetType(Symbol sym) { 
    if(idData.find(sym) != idData.end()) return idData[sym].returnType;
    assert(parentTable != NULL);
    return parentTable->GetType(sym);
}

int SymTable::GetDeclarationLine(Symbol sym) {
    if(idData.find(sym) != idData.end()) return idData[sym].declarationLine;
    if(parentTable == nullptr) return 0;
    return parentTable->GetDeclarationLine(sym);
}

SymTable* SymTable::GetParentTable() { return parentTable; }

string SymTable::GetPath() { return tablePath; }

map<int, string> SymTable::GetTable() {
    map<int, string> output;
    for(auto [symbol, info] : idData) {
        if(info.idType != B) {
            output[info.declarationLine] = "\t| " + tablePath + '/' + symbol.name;
            switch(info.idType) {
                case V:
                    output[info.declarationLine].append(" -> " + info.returnType);
                    break;
                case F:
                    output[info.declarationLine].append(" ( ");
                    if(symbol.params.size() > 0) {
                        output[info.declarationLine].append(idData[symbol.params[0]].returnType);
                        for(int i = 1; i < symbol.params.size(); i ++) {
                            output[info.declarationLine].append(" , " + idData[symbol.params[i]].returnType);
                        }
                    }
                    output[info.declarationLine].append(" ) -> " + info.returnType);
                    break;
                case C:
                    output[info.declarationLine].append(" class");
                    break;
                default: assert(false);
            }
            
            output[info.declarationLine].push_back('\n');
        }

        if(info.idType != V) {
            if(childrenTables[symbol] == nullptr) {
                cout << "(null table) " + GetPath() + " | " + symbol.name + '\n';
                continue;
            }
            map<int, string> temp = childrenTables[symbol]->GetTable();
            for(auto [i, s] : temp)
                output[i] = s;
        }
    }

    return output;
}

string SymTable::String() {
    map<int, string> output = GetTable();
    string ans;
    for(auto [l, s] : output) {
        ans.append(to_string(l) + s);
    }
    return ans;
}