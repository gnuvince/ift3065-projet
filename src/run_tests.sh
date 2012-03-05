#!/bin/bash

IFS=","
TESTS=(
    'gsi test_lexer.scm'
    'gsi test_ast.scm'
)

for t in ${TESTS[@]}
do
    IFS=" "
    ${t[@]}
    IFS=","
done
