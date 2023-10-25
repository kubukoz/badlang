#!/bin/bash

# run file next to this script file's location
# $(dirname "$0")/badlang-lsp
cs launch tech.neander:langoustine-tracer_3:0.0.21 -- --port 8082 "$(dirname "$0")/badlang-lsp"
