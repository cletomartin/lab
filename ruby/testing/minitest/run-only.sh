#!/bin/bash

# $1 can be a regular expression (e.g. /.*to_s.*/)

ruby -I .. test_roman.rb -n "$1"
