#include <vector>
#include <string>
#include <cassert>
#include <map>

using namespace std;

enum IDTYPE { C, F, B, V };

struct Info {
    IDTYPE idType;
    string returnType;
    int declarationLine;
    vector<string> params;

    vector<string> values;
    map<string, string> memberValues;
};

class SymTable {
    map<string, Info> idData;
    map<string, SymTable*> childrenTables;

    SymTable* parentTable;

    string tablePath;
public:
    SymTable(string tableName, SymTable* parentTable = NULL);
    int InsertVariable(int line, string name, string type, int size);
    int InsertFunction(int line, string name, string type, vector<string> params, SymTable* functionTable);
    int InsertClass(int line, string name, SymTable* classTable);
    int InsertBlock(int line, string name, SymTable* blockTable);

    bool IsDefined(string name);
    bool ExistsInScope(string name);

    string GetType(string name);
    vector<string> GetParams(string name);
    int GetDeclarationLine(string name);
    SymTable* GetParentTable();
    SymTable* GetSymTable(string name);
    string GetPath();

    map<int, string> GetTable();
    string String();
};