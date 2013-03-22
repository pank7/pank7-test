#!/usr/bin/env python2

import sys
import struct

fmt = '2048B'

rf = open('/dev/random', 'rb')

c = dict()

for i in xrange(1024):
    for n in struct.unpack(fmt, rf.read(struct.calcsize(fmt))):
        if n in c:
            c[n] += 1
        else:
            c[n] = 1

for k, v in c.iteritems():
    print k, v
