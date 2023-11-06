#!/bin/bash

scala-cli package src -o badlang-lsp --main-class badlang.main -f $@
