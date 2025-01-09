#include <fstream>
#include <vector>
#include <string>
#include <map>

using namespace std;

enum IDTYPE { C, F, B, V };

struct Symbol {
    string name;
    string returnType;
    vector<Symbol> params;

    friend bool operator <(const Symbol& a, const Symbol& b) {
        if(a.name != b.name) return a.name < b.name;
        if(a.returnType != b.returnType) return a.returnType < b.returnType;
        if(a.params.size() != b.params.size()) return a.params.size() != b.params.size();
        for(int i = 0; i < a.params.size(); i ++)
            if(a.params[i].returnType != b.params[i].returnType) return a.params[i] < b.params[i];
        return false;
    }
};

struct Info {
    IDTYPE type;
    int declarationLine;
    // vector<string> calls;
};

class SymTable {
    static ofstream fout;
    
    map<Symbol, Info> idData;
    
    map<Symbol, SymTable*> childrenTables;
    SymTable* parentTable;

    string tablePath;
public:
    SymTable(string tableName, SymTable* parentTable = NULL);
    bool InsertVariable(int line, string name, string type);
    bool InsertFunction(int line, string name, string type, vector<Symbol> params, SymTable* functionTable);
    bool InsertClass(int line, string name, SymTable* classTable);
    bool InsertBlock(int line, string name, SymTable* blockTable);

    SymTable* GetParentTable();
    string GetPath();
    void Print();
};