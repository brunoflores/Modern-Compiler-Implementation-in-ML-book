#!/bin/bash

fswatch -r *.ml lexer.mll test/*.{exp,in,ml} | while read events; do
  make test;
done
