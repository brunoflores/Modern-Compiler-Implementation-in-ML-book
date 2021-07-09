#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

for test in *.tiger; do
  fname_out=$(basename $test ".tiger").out
  fname_exp=$(basename $test ".tiger").exp

  esy dune exec --no-build ./lexer_driver.exe $test > $fname_out
  if [ $? -ne 0 ]; then
    echo ""
    echo "-------------------------"
    printf "${RED}FAILED:${NC} $test\n"
    echo "-------------------------"
    exit 0
  fi

  # Promote:
  # cp $fname_out $fname_exp

  DIFF=$(diff $fname_out $fname_exp)
  if [ "$DIFF" != "" ]; then
    echo ""
    echo "-------------------------"
    printf "${RED}FAILED DIFF:${NC} $test\n"
    echo $DIFF
    echo "-------------------------"
    exit 0
  else
    printf "${GREEN}PASSED:${NC} $test\n"
  fi
done

exit 0
