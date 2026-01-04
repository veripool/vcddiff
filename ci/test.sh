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

! ./vcddiff tests/counter.vcd tests/counter.vcd | grep -q .
! ./vcddiff tests/counter.vcd tests/counter.time.no_diff.vcd | grep -q .
! ./vcddiff tests/counter.vcd tests/counter.change_reorder.no_diff.vcd | grep -q .
! ./vcddiff tests/counter.vcd tests/counter.var_reorder.no_diff.vcd | grep -q .
! ./vcddiff tests/counter.vcd tests/counter.identifier.no_diff.vcd | grep -q .
! ./vcddiff tests/counter.vcd tests/counter.scope_move.no_diff.vcd | grep -q .
! ./vcddiff tests/counter_with_strings.vcd tests/counter_with_strings.identifier.no_diff.vcd | grep -q .
if [ "$FIFO_SUPPORTED" = "1" ]; then
  ! ./vcddiff <(cat tests/counter.vcd) <(cat tests/counter.vcd) | grep -q .
fi

./vcddiff tests/counter.vcd tests/counter.end_time.diff.vcd |
    grep "Files have different end times"
./vcddiff tests/counter.vcd tests/counter.edge_time.diff.vcd |
    grep "t.clk .* at time 20 next occurence at time 21"
./vcddiff tests/counter.vcd tests/counter.sig_name.diff.vcd |
    grep "not defined in both files"
./vcddiff tests/counter.vcd tests/counter.new_sig.diff.vcd |
    grep "Ignoring signal t.the_sub.new_sig .* - not defined in both files"
! ./vcddiff tests/counter_with_strings.vcd tests/counter_with_strings.value_change.diff.vcd |
    grep "t.cyc_string (') differs at time 10"
if [ "$FIFO_SUPPORTED" = "1" ]; then
  ./vcddiff <(cat tests/counter.vcd) <(cat tests/counter.end_time.diff.vcd) |
      grep "Files have different end times"
fi
