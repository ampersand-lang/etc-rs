#!/bin/sh
echo $(cat grammar.ebnf) | sed -e "s/\(.\{60\}\w\+\)/\1\n/g"
