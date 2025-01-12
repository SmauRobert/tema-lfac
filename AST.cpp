#include "AST.hpp"

#include <iostream>

set<string> AST::arithmetic = { "+", "-", "/", "*" };
set<string> AST::logical = { "<", "<=", "==", "!=", ">=", ">", "&&", "||"};

AST::AST(string value, string type, string text) : value(value), type(type), text(text), left(nullptr), right(nullptr), op("") {
    if(type == "void") return;
    if(type == "char" || type == "string") {
        // if(value == "") return;

        // string temp = value.substr(1);
        // temp.pop_back();
        // value = temp;
        return;   
    }
    if(value != "") return;

    if(type == "float" || type == "int") value = "0";
    if(type == "bool") value = "false"; 
}

AST::AST(AST* left, AST* right, string op) : left(left), right(right), op(op) {
    assert(left && right);
    assert(left->GetType() == right->GetType());

    text = left->GetText() + op + right->GetText();

    if(arithmetic.find(op) != arithmetic.end()) {
        type = left->GetType();
        return;
    }

    if(logical.find(op) != logical.end()) {
        type = "bool";
        return;
    }

    // cout << op << '\n';
    assert(false);
}

AST::AST(AST* left, string op) : left(left), op(op), right(nullptr) {
    assert(left && left->GetType() == "bool");
    type = "bool";
    text = "!" + left->GetText();
}

string AST::GetValue() { return value; }
string AST::GetType() { return type; }
string AST::GetText() { return text; }

string AST::Compare(AST* left, AST* right, string op) {
    if(left->GetValue() == "" || right->GetValue() == "") return "";
    if(op == "==") return left->GetValue() == right->GetValue() ? "true" : "false";
    if(op == "!=") return left->GetValue() != right->GetValue() ? "true" : "false";
    if(op == "&&") return left->GetValue() == "true" && right->GetValue() == "true" ? "true" : "false";
    if(op == "||") return left->GetValue() == "true" || right->GetValue() == "true" ? "true" : "false";

    int a = stoi(left->GetValue());
    int b = stoi(right->GetValue());

    if(op == ">") return a > b ? "true" : "false";
    if(op == ">=") return a >= b ? "true" : "false";
    if(op == "<=") return a <= b ? "true" : "false";
    if(op == "<") return a < b ? "true" : "false";

    return "";
}

string AST::Compute(AST* left, AST* right, string op) {
    float a = stoi(left->GetValue());
    float b = stoi(right->GetValue());

    float result;
    if(op == "+") result = a + b;
    if(op == "-") result = a - b;
    if(op == "/") {
        assert(b != 0);
        if(left->GetType() == "int") result = (int)(int(a) / int(b));
        else result = a / b;
    }
    if(op == "*") result = a * b;

    if(left->GetType() == "int") 
        return to_string(int(result));
    return to_string(result);
}

void AST::ComputeValue() {
    if(left == nullptr) { /* TODO: retrieve the value from the symbol table (if there is any)*/
        return;
    }
    left->ComputeValue();

    if(right == nullptr) {
        if(left->GetValue() == "true") value = "false";
        else value = "true";
        return;
    }

    right->ComputeValue();
    if(arithmetic.find(op) != arithmetic.end()) {
        value = Compute(left, right, op);
        return;
    }

    if(logical.find(op) != logical.end()) {
        type = "bool";
        value = Compare(left, right, op);
        return;
    }
    // cout << op << '\n';
    assert(false);
}