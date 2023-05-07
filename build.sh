#!/bin/bash

if [ -z "$NATIVE" ]
then
  echo "Building jvm"
  scala-cli package . -o badlang-lsp --main-class badlang.main -f $@
else
  echo "Building native"
  scala-cli package . -o badlang-lsp --main-class badlang.main -f --native $@
fi
