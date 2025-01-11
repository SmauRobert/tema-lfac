#include <vector>
#include <string>
#include <cassert>
#include <map>

using namespace std;

enum IDTYPE { C, F, B, V };

struct Symbol;

struct Info {
    IDTYPE idType;
    string returnType;
    int declarationLine;
};

class SymTable {
    
    map<Symbol, Info> idData;
    map<Symbol, SymTable*> childrenTables;

    SymTable* parentTable;

    string tablePath;
public:
    SymTable(string tableName, SymTable* parentTable = NULL);
    int InsertVariable(int line, string name, string type);
    int InsertFunction(int line, string name, string type, vector<Symbol> params, SymTable* functionTable);
    int InsertClass(int line, string name, SymTable* classTable);
    int InsertBlock(int line, string name, SymTable* blockTable);

    bool IsVariableDefined(string name);

    string GetType(Symbol s);
    int GetDeclarationLine(Symbol sym);
    SymTable* GetParentTable();
    string GetPath();
    
    map<int, string> GetTable();
    string String();
};

struct Symbol {
    string name;
    SymTable* parentScope;
    vector<Symbol> params;

    friend bool operator <(const Symbol& a, const Symbol& b) {
        if(a.name != b.name) return a.name < b.name;
        if(a.params.size() != b.params.size()) return a.params.size() < b.params.size();
        for(int i = 0; i < a.params.size(); i ++) {
            Symbol x = a.params[i];
            Symbol y = b.params[i];
            assert(x.parentScope != nullptr);
            assert(y.parentScope != nullptr);
            string aType = x.parentScope->GetType(x);
            string bType = y.parentScope->GetType(y);
            if(aType != bType) return aType < bType;
        }
        return false;
    }
};