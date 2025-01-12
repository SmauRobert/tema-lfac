#include <string>
#include <set>
#include <cassert>

using namespace std;

class AST {
    static set<string> arithmetic;
    static set<string> logical;

    AST *left, *right;
    string op;
    
    string value;
    string type;
    string text;

    string Compute(AST* left, AST* right, string op);
    string Compare(AST* left, AST* right, string op);
public:
    AST() = default;
    AST(string value, string type, string text);
    AST(AST* left, AST* right, string op);
    AST(AST* left, string op);

    void ComputeValue();

    string GetValue();
    string GetType();
    string GetText();
};