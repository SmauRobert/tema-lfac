#!/bin/bash

rm -f lex.yy.c
rm -f parser.tab.*

bison -d parser.y -Wcounterexamples 2> out.err
lex lexer.l
g++ lex.yy.c parser.tab.c -o main