#!/bin/bash

rm -f lex.yy.c
rm -f parser.tab.*

bison -d parser.y
lex lexer.l
g++ SymTable.cpp lex.yy.c parser.tab.c -o main