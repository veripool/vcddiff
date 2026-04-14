#!/bin/bash
# DESCRIPTION: CI test script
# SPDX-License-Identifier: GPL-3.0
# Copyright 2025 Wilson Snyder

set -e
set -x

if [[ "$(uname -s)" != MINGW* && "$(uname -s)" != MSYS* && "$(uname -s)" != CYGWIN* ]]; then
    FIFO_SUPPORTED=1
else
    FIFO_SUPPORTED=0
fi

test -z "$(./vcddiff tests/counter.vcd tests/counter.vcd)"
test -z "$(./vcddiff tests/counter.vcd tests/counter.time.no_diff.vcd)"
test -z "$(./vcddiff tests/counter.vcd tests/counter.change_reorder.vcd)"
test -z "$(./vcddiff tests/counter.vcd tests/counter.var_reorder.vcd)"
test -z "$(./vcddiff tests/counter.vcd tests/counter.identifier.vcd)"
test -z "$(./vcddiff tests/counter.vcd tests/counter.scope_move.no_diff.vcd)"
if [ "$FIFO_SUPPORTED" = "1" ]; then
    test -z "$(./vcddiff <(cat tests/counter.vcd) <(cat tests/counter.vcd))"
fi

./vcddiff tests/counter.vcd tests/counter.end_time.diff.vcd |
    grep "Files have different end times"
./vcddiff tests/counter.vcd tests/counter.edge_time.diff.vcd |
    grep "t.clk .* at time 20 next occurence at time 21"
./vcddiff tests/counter.vcd tests/counter.sig_name.diff.vcd |
    grep "not defined in both files"
./vcddiff tests/counter.vcd tests/counter.new_sig.diff.vcd |
    grep "Ignoring signal t.the_sub.new_sig .* - not defined in both files"
if [ "$FIFO_SUPPORTED" = "1" ]; then
    ./vcddiff <(cat tests/counter.vcd) <(cat tests/counter.end_time.diff.vcd) |
        grep "Files have different end times"
fi

# Bug: vcddiff reports false differences when identifiers are shared across
# scopes in one file but unique in the other (wavediff handles this correctly)
test -z "$(./vcddiff tests/idcode_a.vcd tests/idcode_b.vcd)"

./vcddiff --version |
    grep "vcddiff"
./vcddiff -v |
    grep "vcddiff"
./vcddiff -V |
    grep "vcddiff"

echo "== Tests passed"
