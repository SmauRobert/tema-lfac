#include "SymTable.hpp"

SymTable::SymTable(string tableName, SymTable* parentTable) : parentTable(parentTable) {
    if(parentTable == nullptr) {
        tablePath = tableName;
        return;
    }

    tablePath = parentTable->GetPath() + "/" + tableName;
}

int SymTable::InsertVariable(int line, string name, string type, int size) {
    if(ExistsInScope(name)) return GetDeclarationLine(name);

    idData[name] = { V, type, line, {}, vector<string>(size) };

    return 0;
}

int SymTable::InsertFunction(int line, string name, string type, vector<string> params, SymTable* functionTable) {
    if(ExistsInScope(name)) return GetDeclarationLine(name);
    
    idData[name] = { F, type, line, params };
    childrenTables[name] = functionTable;
    return 0;
}

int SymTable::InsertClass(int line, string name, SymTable* classTable) {
    if(ExistsInScope(name)) return GetDeclarationLine(name);

    idData[name] = { C, name, line, {} }; 
    childrenTables[name] = classTable;
    return 0;
}

int SymTable::InsertBlock(int line, string name, SymTable* blockTable) {
    if(ExistsInScope(name)) return GetDeclarationLine(name);
    
    idData[name] = { B, "void", line, {} };
    childrenTables[name] = blockTable;
    return 0;
}

bool SymTable::IsDefined(string name) {
    if(idData.find(name) != idData.end()) return true;
    if(parentTable == nullptr) return false;
    return parentTable->IsDefined(name);
}

bool SymTable::ExistsInScope(string name) {
    return idData.find(name) != idData.end();
}

string SymTable::GetType(string name) { 
    if(idData.find(name) != idData.end()) { 
        string type = idData[name].returnType;
        if(idData[name].values.capacity() > 1)
            type.append('[' + to_string(idData[name].values.capacity()) + ']');
        return type;
    }
    assert(parentTable != nullptr);
    return parentTable->GetType(name);
}

vector<string> SymTable::GetParams(string name) {
    if(idData.find(name) != idData.end()) return idData[name].params;
    return vector<string>();
}

int SymTable::GetDeclarationLine(string name) {
    if(idData.find(name) != idData.end()) return idData[name].declarationLine;
    if(parentTable == nullptr) return 0;
    return parentTable->GetDeclarationLine(name);
}

SymTable* SymTable::GetSymTable(string name) {
    auto it = childrenTables.find(name);
    if(it != childrenTables.end()) return it->second;
    if(parentTable == nullptr) return nullptr;
    return parentTable->GetSymTable(name);
}

string SymTable::GetValue(string name, string field) {
    auto it = idData.find(name);
    if(it != idData.end()) return it->second.memberValues[field];
    if(parentTable != nullptr) return "";
    return parentTable->GetValue(name, field);
}

void SymTable::SetValue(string name, string value, int index) {
    auto it = idData.find(name);
    if(it != idData.end()) {
        idData[name].values[index] = value;
        return;
    }
    parentTable->SetValue(name, value, index);
}

string SymTable::GetValue(string name, int index) {
    auto it = idData.find(name);
    if(it != idData.end()) return it->second.values[index];
    if(parentTable != nullptr) return "";
    return parentTable->GetValue(name, index);
}

SymTable* SymTable::GetParentTable() { return parentTable; }

string SymTable::GetPath() { return tablePath; }

map<int, string> SymTable::GetTable() {
    map<int, string> output;
    for(auto [name, info] : idData) {
        if(info.idType != B) {
            output[info.declarationLine] = "\t| " + tablePath + '/' + name;
            switch(info.idType) {
                case V:
                    output[info.declarationLine].append(" -> " + info.returnType);
                    if(info.values.capacity() > 1)
                        output[info.declarationLine].append('[' + to_string(info.values.capacity()) + ']');
                    // else output[info.declarationLine].append(" = " + info.values[0]);
                    break;
                case F:
                    output[info.declarationLine].append(" ( ");
                    if(info.params.size() > 0) {
                        output[info.declarationLine].append(childrenTables[name]->GetType(info.params[0]));
                        for(int i = 1; i < info.params.size(); i ++)
                            output[info.declarationLine].append(" , " + childrenTables[name]->GetType(info.params[i]));
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
            map<int, string> temp = childrenTables[name]->GetTable();
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