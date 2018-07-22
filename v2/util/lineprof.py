#!/usr/bin/env python3

# tracks the time for each line to be read into stdin,
# plots a bargraph with the top 10 slowest lines

# useful for things like profiling slow compilation with
#
#   dub -v ... | lineprof.py
#

import sys
import time

r = []

while True:
    b = time.time()
    line = sys.stdin.readline()
    if not line:
        break
    e = time.time()
    sys.stdout.write(line)
    r.append(((e - b), line))

import matplotlib.pyplot as plt
r = sorted(r, key=lambda x: x[0], reverse=True)[:10]
plt.barh([x[1] for x in r], [x[0] for x in r])
plt.show()
