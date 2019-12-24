#!/bin/sh

set -e
set -x
this_dir=$(dirname $0)

md_files="simple-expressions.md simple-functions.md unions.md type-classes.md structs.md refs.md"

$this_dir/swipl -f $this_dir/neutrino.pl -t run_tests

python3 neutrino_testing_test.py
python3 neutrino_testing.py $md_files
