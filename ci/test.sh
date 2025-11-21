#!/bin/bash
# DESCRIPTION: CI test script
# SPDX-License-Identifier: GPL-3.0
# Copyright 2025 Wilson Snyder

set -e
set -x

! ./vcddiff tests/counter.vcd tests/counter.vcd | grep -q .
! ./vcddiff tests/counter.vcd tests/counter.time.no_diff.vcd | grep -q .
! ./vcddiff tests/counter.vcd tests/counter.change_reorder.no_diff.vcd | grep -q .
! ./vcddiff tests/counter.vcd tests/counter.var_reorder.no_diff.vcd | grep -q .
! ./vcddiff tests/counter.vcd tests/counter.identifier.no_diff.vcd | grep -q .

./vcddiff tests/counter.vcd tests/counter.end_time.diff.vcd |
    grep "Files have different end times"
./vcddiff tests/counter.vcd tests/counter.edge_time.diff.vcd |
    grep "t.clk .* at time 20 next occurence at time 21"
./vcddiff tests/counter.vcd tests/counter.sig_name.diff.vcd |
    grep "not defined in both files"
./vcddiff tests/counter.vcd tests/counter.new_sig.diff.vcd |
    grep "Ignoring signal t.the_sub.new_sig .* - not defined in both files"
