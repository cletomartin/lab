#!/bin/bash

# e.g. ./run-only.sh "fails if a non-integer is provided"

rspec -I .. . -c -f d -e "$1"
