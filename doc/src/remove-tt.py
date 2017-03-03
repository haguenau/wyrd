#!/usr/bin/env python
# Strip out all the \texttt{} occurrences (not perfect, but good enough)
import re, sys

in_filename, out_filename = sys.argv[1:]
tt_regex = re.compile(r'\\texttt\{([^\}]+)\}')

def tt_replace(m):
    return m.group(1) 

with open(in_filename, 'r') as in_file:
    orig = in_file.read()

replaced = tt_regex.sub(tt_replace, orig)
with open(out_filename, 'w') as out_file:
    out_file.write(replaced)

