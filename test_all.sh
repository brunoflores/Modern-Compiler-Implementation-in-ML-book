#!/bin/bash

for test in test/*.in; do
  esy dune exec ./test/lexer_driver.exe $test > test/$(basename $test ".in").out
  if [ $? -ne 0 ]; then
    echo ""
    echo "-------------------------"
    echo "FAILED: $test"
    echo "-------------------------"
    exit 0
  fi
  DIFF=$(diff "test/$(basename $test ".in").out" "test/$(basename $test ".in").exp")
  if [ "$DIFF" != "" ]; then
    echo ""
    echo "-------------------------"
    echo "FAILED: $test"
    echo $DIFF
    echo "-------------------------"
    exit 0
  else
    echo ""
    echo "-------------------------"
    echo "PASSED: $test"
    echo "-------------------------"
    echo ""
  fi
done

exit 0
