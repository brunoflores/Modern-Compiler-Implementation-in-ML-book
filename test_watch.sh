#!/bin/bash

fswatch -r parser.mly lexer.mll *.{ml,mli} test/**/*.{exp,tiger,ml,mli} | while read events; do
  make test
  echo ""
  echo ""
done
