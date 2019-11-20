#!/bin/sh

set -e

python3 neutrino_testing_test.py
python3 neutrino_testing.py type-system.md
