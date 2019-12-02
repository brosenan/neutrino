#!/bin/sh

set -e

md_files="type-system.md unions.md"

python3 neutrino_testing_test.py
python3 neutrino_testing.py $md_files
