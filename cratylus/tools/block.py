#!/usr/bin/python2
import re
import sys

if len(sys.argv) != 2:
    print 'Usage: %s file.in' % (sys.argv[0],)
    print 'Compact a Cratylus program into a yet more unreadable block.'
    sys.exit(1)

f = file(sys.argv[1], 'r')
prog = f.read()
f.close()

tot = ''
lin = ''
sep = ''
for l in prog.split('\n'):
    l = re.sub(' ', '', l)
    if len(lin) + len(sep) + len(l) > 80:
        tot = tot + '\n' + lin
        lin = l
    else:
        lin += sep + l
    sep = ''

tot = tot + '\n' + lin
print tot

