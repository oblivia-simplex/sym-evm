#!/usr/local/bin/python

from collections import Counter
import sys

opcode_file = open(sys.argv[1], 'r')
contents = opcode_file.read()
opcode_file.close()

words = contents.split()
instrs = filter(lambda s: s[0:2] != "0x", words)
counts = Counter(instrs)

for k, v in counts.iteritems():
    print k, v
