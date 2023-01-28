#!/bin/bash

# script to find any non-zero error processes
find . -name \*.err ! -size 0 -ls | sort -n -n -k7 | cut -d'_' -f 3 | sort -n | cut -d'.' -f 1