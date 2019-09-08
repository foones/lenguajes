#!/usr/bin/python2
import sys
import re

MAX_BITS = 64
UNROLL_BITS = 4

def range_of_bits(lower=0, upper=MAX_BITS, full=False):
    if full:
        unroll = 1
    else:
        unroll = UNROLL_BITS
    return [2 ** x for x in reversed(range(lower, upper, unroll))]

class S2CrException(Exception):
    pass

def is_numeric(x):
    for c in x:
        if c not in '0123456789':
            return False
    return True

def xzero(var, line1, line2):
    result = []
    result.append('{%s}{%s}^@ => {%s}' % (line1, var, line2))
    result.append('{%s} => {%s}' % (line1, line2))
    return result

def xmov(src, dst, line1, line2):
    result = []
    if is_numeric(src):
        result.append('{%s}{%s}^@ => {%s}{%s}^%u' % (line1, dst, line2, dst, int(src)))
        result.append('{%s} => {%s}{%s}^%u' % (line1, line2, dst, int(src)))
    else:
        result.append('{%s}{%s}^@ => {%s}' % (line1, dst, line1))
        result.append('{%s}{%s}^@ => {%s}{%s}^@{%s}^@' % (line1, src, line2, src, dst))
        result.append('{%s} => {%s}' % (line1, line2))
    return result

def xadd(src, dst, line1, line2):
    result = []
    if is_numeric(src):
        result.append('{%s} => {%s}{%s}^%u' % (line1, line2, dst, int(src)))
    else:
        result.append('{%s}{%s}^@ => {%s}{%s}^@{%s}^@' % (line1, src, line2, src, dst))
        result.append('{%s} => {%s}' % (line1, line2))
    return result

def xsub(src, dst, line1, line2):
    result = []
    if is_numeric(src):
        result.append('{%s} => {%s,a}{,1}^%u' % (line1, line1, int(src)))
        result.append('{%s,a}{%s}^@{,1}^@ => {%s,b}' % (line1, dst, line1))
        result.append('{%s,a} => {%s,b}' % (line1, line1))
        result.append('{%s,b}{,1}^@ => {%s}' % (line1, line2))
        result.append('{%s,b} => {%s}' % (line1, line2))
    else:
        result.append('{%s}{%s}^@ => {%s,a}{,1}^@{%s}^@' % (line1, src, line1, src))
        result.append('{%s} => {%s}' % (line1, line2))
        result.append('{%s,a}{%s}^@{,1}^@ => {%s,b}' % (line1, dst, line1))
        result.append('{%s,a} => {%s,b}' % (line1, line1))
        result.append('{%s,b}{,1}^@ => {%s}' % (line1, line2))
        result.append('{%s,b} => {%s}' % (line1, line2))
    return result

def divmod_pow2(var, remainder, nbits, line1, line2):
    result = []
    if nbits == 0:
        result.append('{%s} => {%s}' % (line1, line2))
    else:
        np = 2 ** nbits

        # x var
        # R    ,1
        # y    ,2
        # r    ,3
        # z    ,4
        # z_2  ,5
        # x_2  ,6
        #
        ###
        #
        # r = 1 
        # y = p
        # while (true) {     ,a
        #    z = y^2
        #    if z > x: break
        #    y = z
        #    r = r^2
        # }
        # x = x - y
        # R = R + r
        #
        # return R
        #
        ###

        result.append('{%s}{%s} => {%s,0}{%s}' % (line1, var, line1, var))
        result.append('{%s} => {%s,l}' % (line1, line1))
        result.append('{%s,0} => {%s,a}{,2}^%u{,3}' % (line1, line1, np))

        # z <- y^2
        result.append('{%s,a}{,2}^@ => {%s,b}{,2}^@{,4}^@{,5}^@' % (line1, line1))
        result.append('{%s,b}{,5}^@ => {%s,c}{,4}^@' % (line1, line1))

        # z --> z_2
        result.append('{%s,c}{,4}^@ => {%s,d}{,4}^@{,5}^@' % (line1, line1))
        # x --> x_2
        result.append('{%s,d}{%s}^@ => {%s,e}{%s}^@{,6}^@' % (line1, var, line1, var))
        result.append('{%s,d} => {%s,e}' % (line1, line1))

        # cmp x_2 z_2
        result.append('{%s,e}{,5}^@{,6}^@ => {%s,f}' % (line1, line1))
        result.append('{%s,f}{,5}^@ => {%s,j}' % (line1, line1))
        result.append('{%s,f}{,6}^@ => {%s,f}' % (line1, line1))
        result.append('{%s,f}{,2}^@ => {%s,g}' % (line1, line1))
        result.append('{%s,f} => {%s,g}' % (line1, line1))
        result.append('{%s,g}{,4}^@ => {%s,h}{,2}^@' % (line1, line1))
        result.append('{%s,g} => {%s,h}' % (line1, line1))
        result.append('{%s,h}{,3}^@ => {%s,i}{,5}^@{,6}^@' % (line1, line1))
        result.append('{%s,h} => {%s,i}' % (line1, line1))
        result.append('{%s,i}{,5}^@ => {%s,i}{,3}^@' % (line1, line1))
        result.append('{%s,i}{,6}^@ => {%s,a}{,3}^@' % (line1, line1))
        result.append('{%s,i} => {%s,a}' % (line1, line1))

        result.append('{%s,j}{,4}^@ => {%s,j}' % (line1, line1))
        result.append('{%s,j}{%s}^@{,2}^@ => {%s,k}' % (line1, var, line1))
        result.append('{%s,k}{,2}^@ => {%s,l}{%s}^@' % (line1, line1, remainder))
        result.append('{%s,k}{,3}^@ => {%s}{,1}^@' % (line1, line1))
        result.append('{%s,l}{,3}^@ => {%s,l}' % (line1, line1))
        result.append('{%s,l}{,1}^@ => {%s,l}{%s}^@' % (line1, line1, var))
        result.append('{%s,l} => {%s}' % (line1, line2))
    return result

def xshl(var, nbits, line1, line2):
    result = []
    if nbits == 0:
        result.append('{%s} => {%s}' % (line1, line2))
    else:
        result.append('{%s} => {%s,a}{,1}^%u' % (line1, line1, nbits))
        result.append('{%s,a}{,1}{%s}^@ => {%s,b}{,2}^@{,3}^@' % (line1, var, line1))
        result.append('{%s,a}{,1}^@ => {%s}' % (line1, line2))
        result.append('{%s,a} => {%s}' % (line1, line2))
        result.append('{%s,b}{,2}^@ => {%s,b}{%s}^@' % (line1, line1, var))
        result.append('{%s,b}{,3}^@ => {%s,a}{%s}^@' % (line1, line1, var))
    return result

def s_to_cratylus(string):

    string = re.sub('[ \t]+', ' ', string)
    lines = string.split('\n')

    numline = 0
    labels_to_numlines = {}
    initial_values = {}

    # Preprocess labels
    instructions = []
    for line in lines:
        line = line.split('#')[0].strip(' \t\r\n')
        if line == '':
            continue

        if line[-1] == ':':
            label = line[:-1]
            if label in labels_to_numlines:
                raise S2CrException('label "%s" should not occur twice' % (label,))
            labels_to_numlines[label] = numline
        elif line[0] == '!':
            line = line.split(' ')
            if len(line) != 3 or not is_numeric(line[2]): 
                raise S2CrException('variable initialization should be of the form "! <var> <value>"')

            var = line[1]
            value = int(line[2])

            if var in initial_values:
                raise S2CrException('variable "%s" is initialized twice (%u, %u)' % (var, initial_values[var], value))

            initial_values[var] = value
        else:
            instructions.append(line)
            numline += 1

    # Compile instructions
    result = []
    numline = 0
    for op in instructions:
        op = [x.strip(' \t\r\n') for x in op.split(' ')]

        if op[0] in ['inc', 'dec', 'jmp', 'goto', 'xzero'] and len(op) != 2:
            raise S2CrException('operation "%s" takes exactly one argument' % (op[0],))

        if op[0] in ['jz', 'jnz', 'xmov', 'xadd', 'xsub', 'xshr','xshl'] and len(op) != 3:
            raise S2CrException('operation "%s" takes exactly two arguments' % (op[0],))

        if op[0] in ['xshr_rem'] and len(op) != 4:
            raise S2CrException('operation "%s" takes exactly three arguments' % (op[0],))

        if op[0] in ['jmp', 'goto', 'jz', 'jnz']:
            label = op[-1]
            if label not in labels_to_numlines:
                raise S2CrException('undefined label "%s"' % (label,))

        if op[0] == 'inc':
            var = op[1]
            result.append('{%u} => {%u}{%s}' % (numline, numline + 1, var))

        elif op[0] == 'dec':
            var = op[1]
            result.append('{%u}{%s} => {%u}' % (numline, var, numline + 1))
            result.append('{%u} => {%u}' % (numline, numline + 1))

        elif op[0] in ['jmp', 'goto']:
            label = op[1]
            result.append('{%u} => {%u}' % (numline, labels_to_numlines[label]))

        elif op[0] in ['jnz']:
            var = op[1]
            label = op[2]
            result.append('{%u}{%s} => {%u}{%s}' % (numline, var, labels_to_numlines[label], var))
            result.append('{%u} => {%u}' % (numline, numline + 1))

        elif op[0] in ['jz']:
            var = op[1]
            label = op[2]
            result.append('{%u}{%s} => {%u}{%s}' % (numline, var, numline + 1, var))
            result.append('{%u} => {%u}' % (numline, labels_to_numlines[label]))

        # extensions

        elif op[0] == 'xzero':
            result.extend(xzero(op[1], numline, numline + 1))

        elif op[0] == 'xmov':
            dst = op[1]
            src = op[2]
            result.extend(xmov(src, dst, numline, numline + 1))

        elif op[0] == 'xadd':
            dst = op[1]
            src = op[2]
            result.extend(xadd(src, dst, numline, numline + 1))

        elif op[0] == 'xsub':
            dst = op[1]
            src = op[2]
            result.extend(xsub(src, dst, numline, numline + 1))

        elif op[0] == 'xshr':
            var = op[1]
            nbits = op[2]
            if not is_numeric(nbits):
                raise S2CrException('"xshr": second operand should be a number')

            rem = '%s,r' % (var,)
            linez = '%s,z' % (numline,)
            result.extend(divmod_pow2(var, rem, int(nbits), numline, linez))
            result.extend(xzero(rem, linez, numline + 1))

        elif op[0] == 'xshr_rem':
            var = op[1]
            rem = op[2]
            nbits = op[3]
            if not is_numeric(nbits):
                raise S2CrException('"xshr": second operand should be a number')

            linez = '%s,z' % (numline,)
            result.extend(xzero(rem, numline, linez))
            result.extend(divmod_pow2(var, rem, int(nbits), linez, numline + 1))

        elif op[0] == 'xshl':
            var = op[1]
            nbits = op[2]
            if not is_numeric(nbits):
                raise S2CrException('"xshl": second operand should be a number')
            result.extend(xshl(var, int(nbits), numline, numline + 1))

        numline += 1

    result.append('{%u}' % (numline,))

    goal = ['{0}']
    for var, val in sorted(initial_values.items()):
        goal.append('{%s}^%u' % (var, val))

    result.append('? %s' % (''.join(goal),))

    return '\n'.join(['%s.' % (r,) for r in result])

def usage():
    sys.stderr.write('Dumb S to Cratylus compiler.\n')
    sys.stderr.write('Copyright (c) 2012 - Pablo Barenbaum <foones@gmail.com>\n')
    sys.stderr.write('Usage:\n')
    sys.stderr.write('    %s <infile.s>\n' % (sys.argv[0],))
    sys.exit(1)

if __name__ == '__main__':
    if len(sys.argv) != 2:
        usage()

    in_file = sys.argv[1]

    if in_file.endswith('.s'):
        out_file = in_file[:-2] + '.crm'
    else:
        out_file = in_file + '.crm'

    f = file(sys.argv[1], 'r')
    contents = f.read()
    f.close()

    try:
        result = s_to_cratylus(contents)
    except S2CrException, e:
        print 's2cr:', e
        sys.exit(1) 

    sys.stderr.write(result)

    out = file(out_file, 'w')
    out.write(result)
    out.close()

