#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

#from __future__ import print_function
import sys, os, site, time

if __name__ == '__main__':
    # print(site.getusersitepackages())
    # print(site.getsitepackages())
    g = globals()
    for k, v in g.items():
        print(k, v)
    for i in xrange(10):
        print(i)
    for k, v in enumerate(['a', 'b', 'c']):
        print('%d => %s' % (k, v))
    
