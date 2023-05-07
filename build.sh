#!/bin/bash

if [ -z "$NATIVE" ]
then
  echo "Building jvm"
  scala-cli package . -o badlang-lsp -f $@
else
  echo "Building native"
  scala-cli package . -o badlang-lsp -f --native $@
fi
