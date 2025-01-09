#include "SymTable.hpp"

ofstream SymTable::fout = [](){
    ofstream fout("SymbolTable.output");
    fout << "Symbol Table\n";
    return fout;
}();

SymTable::SymTable(string tableName, SymTable* parentTable) : parentTable(parentTable) {
    if(parentTable == nullptr) {
        tablePath = tableName;
        return;
    }

    tablePath = parentTable->GetPath() + "/" + tableName;
}

bool SymTable::InsertVariable(int line, string name, string type) {
    Symbol newSymbol = { name, type, {} };
    if(idData.find(newSymbol) != idData.end()) return false;

    idData[newSymbol] = { V, line };
    return true;
}

bool SymTable::InsertFunction(int line, string name, string type, vector<Symbol> params, SymTable* functionTable) {
    Symbol newSymbol = { name, type, params };
    if(idData.find(newSymbol) != idData.end()) return false;
    
    idData[newSymbol] = { F, line };
    childrenTables[newSymbol] = functionTable;

    return true;
}

bool SymTable::InsertClass(int line, string name, SymTable* classTable) {
    Symbol newSymbol = { name, name, {} };
    if(idData.find(newSymbol) != idData.end()) return false;

    idData[newSymbol] = { C, line }; 
    childrenTables[newSymbol] = classTable;
    return true;
}

bool SymTable::InsertBlock(int line, string name, SymTable* blockTable) {
    Symbol newSymbol = { name, "void", {} };
    if(idData.find(newSymbol) != idData.end()) return false;
    
    idData[newSymbol] = { B, line };
    childrenTables[newSymbol] = blockTable;
    return true;
}

SymTable* SymTable::GetParentTable() { return parentTable; }
string SymTable::GetPath() { return tablePath; }

void SymTable::Print() {
    for(auto [symbol, info] : idData) {
        if(info.type != B) {
            fout << tablePath + ": " + symbol.name + " -> " + symbol.returnType;
            if(childrenTables.find(symbol) == childrenTables.end()) {
                fout << '\n';
                continue;
            }
            
            if(info.type == F) {
                fout << " ( ";
                if(symbol.params.size() > 0) {
                    fout << symbol.params[0].returnType;
                    for(int i = 1; i < symbol.params.size(); i ++)
                        fout << " , " << symbol.params[i].returnType;
                }
                fout << " )\n";
            } else fout << " class\n";
        }

        childrenTables[symbol]->Print();
    }
}

