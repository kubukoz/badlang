#!/bin/bash

# run file next to this script file's location
cs launch tech.neander:langoustine-tracer_3:0.0.21 -- --port 8082 "$(dirname "$0")/badlang-lsp"

