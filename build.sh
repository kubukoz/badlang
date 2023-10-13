#!/bin/bash

scala-cli package . -o badlang-lsp --main-class badlang.main -f $@
