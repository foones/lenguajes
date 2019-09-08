#!/usr/bin/python2
import sys
import re
import string

class SS2SException(Exception):
    pass

def indent(x):
    return '\n'.join(['    ' + l for l in x.split('\n')])

def extensions():
    return ['xzero', 'xmov', 'xadd', 'xsub', 'xshr', 'xshr_rem', 'xshl']

def is_numeric(xs):
    for x in xs:
        if x not in string.digits:
            return False
    return True

class Program(object):

    def __init__(self, body):
        self.body = body

    def __repr__(self):
        return '\n'.join([repr(x) for x in self.body])

class Macro(object):

    def __init__(self, head, body):
        self.head = head
        self.body = body

    def __repr__(self):
        return '%s\n%s\nEND' % (' '.join(self.head), indent('\n'.join([repr(x) for x in self.body])))

    def is_macro(self):
        return True

class Instruction(object):

    def __init__(self, op):
        self.op = op

    def __repr__(self):
        return ' '.join(self.op)

    def is_macro(self):
        return False

class Sub(object):

    def __init__(self, name, params, body):
        self.name = name
        self.params = params
        self.body = body
        self._times_used = 0

    def __repr__(self):
        return 'SUB %s %s\n%s\nEND' % (
            self.name,
            ' '.join(self.params),
            indent('\n'.join([repr(x) for x in self.body]))
        )

    def make_prefix(self):
        self._times_used += 1
        return self.name + ':%u' % (self._times_used,)

def all_upper(name):
    for x in name:
        if x not in string.uppercase:
            return False
    return True

def mangle(prefix, name):
    if prefix == '':
        return name
    else:
        return '%s:%s' % (prefix, name)

LABEL_COUNT = 0
def make_label(prefix):
    global LABEL_COUNT
    LABEL_COUNT += 1
    return ':l:%u' % (LABEL_COUNT,)

def ss_tree(program):
    re.sub('[ \t]+', ' ', program)
    lines = program.split('\n')

    pending = []
    stack = [[]]
    for line in lines:
        assert len(stack) == len(pending) + 1

        line = line.split('#')[0].strip(' \t\r\n')
        if line == '':
            continue
        line = line.split(' ')
        if all_upper(line[0]):
            if line[0] == 'END':
                if len(line) != 1:
                    raise SS2SException('"END" should be left alone in a line')
                if pending == []:
                    raise SS2SException('closing "END" does not match an opening macro')
                head = pending.pop(-1)
                body = stack.pop(-1)
                stack[-1].append(Macro(head, body))
            else:
                stack.append([])
                pending.append(line)
        else:
            stack[-1].append(Instruction(line))

    if len(stack) > 1:
        raise SS2SException('opening macro: "%s" not closed' % (pending[-1][0],))
        
    return Program(stack[0])

def collect_subroutines(body, subroutines):
    for instr in body:
        if instr.is_macro() and instr.head[0] == 'SUB':
            if len(instr.head) < 2: 
                raise SS2SException('subroutine declaration should have a name')
            subroutines[instr.head[1]] = Sub(instr.head[1], instr.head[2:], instr.body)

def env_expand1(env, prefix, name):
    if is_numeric(name):
        return name 
    elif name in env:
        return env[name]
    else:
        return mangle(prefix, name)

def env_expand(env, prefix, names):
    res = []
    for name in names:
        res.append(env_expand1(env, prefix, name))
    return res

def compile_control(control_type, instr, subroutines, env, prefix):
    if len(instr.head) != 2:
        raise SUB('macro "%s" expects an argument' % (control_type,))

    result = []
    var = env_expand1(env, prefix, instr.head[1])
    lstart = make_label(prefix)
    lend = make_label(prefix)

    if control_type.endswith('NZ'):
        jump = 'jz'
    else:
        jump = 'jnz'

    if control_type.startswith('WHILE'):
        result.append('%s:' % (lstart,))
    result.append('    %s %s %s' % (jump, var, lend))
    result.extend(compile_body(instr.body, subroutines, env, prefix))
    if control_type.startswith('WHILE'):
        result.append('    jmp %s' % (lstart))
    result.append('%s:' % (lend,))
    return result

def compile_body(body, subroutines, env, prefix):
    result = []
    for instr in body:
        if instr.is_macro():
            if instr.head[0] == 'SUB':
                pass
            elif instr.head[0] in ['IFZ', 'IFNZ', 'WHILEZ', 'WHILENZ']:
                result.extend(compile_control(instr.head[0], instr, subroutines, env, prefix))
            else:
                raise SUB('macro "%s" not implemented' % (instr.head[0]))
        else:
            if instr.op[0][-1] == ':':
                label = instr.op[0][:-1]
                result.append('%s:' % (mangle(prefix, label),))
            elif instr.op[0].split('/')[0] in ['inc', 'dec', 'jmp', 'goto', 'jz', 'jnz', '!'] + extensions():
                operands = env_expand(env, prefix, instr.op[1:])
                result.append('    %s %s' % (instr.op[0], ' '.join(operands)))
            elif instr.op[0] in subroutines:
                operands = env_expand(env, prefix, instr.op[1:])
                sub = subroutines[instr.op[0]]
                if len(sub.params) != len(operands):
                    raise SS2SException('arity mismatch: subroutine "%s" expects %u, received %u' % (
                        instr.op[0],
                        len(sub.params),
                        len(operands)))
                new_env = dict(zip(sub.params, operands))
                new_prefix = sub.make_prefix()
                result.append('    # %s' % (' '.join(instr.op),))
                result.extend(compile_body(sub.body, subroutines, new_env, new_prefix))
            else:
                raise SS2SException('subroutine "%s" is not declared' % (instr.op[0],))
    return result

def ss_to_s(program):
    program = ss_tree(program)
    subroutines = {}
    collect_subroutines(program.body, subroutines)
    return compile_body(program.body, subroutines, {}, '')

def usage():
    sys.stderr.write('Compile an S-with-macros program into a plain S program.\n')
    sys.stderr.write('Copyright (c) 2012 - Pablo Barenbaum <foones@gmail.com>\n')
    sys.stderr.write('Usage:\n')
    sys.stderr.write('    %s <infile.ss>\n' % (sys.argv[0],))
    sys.exit(1)

if __name__ == '__main__':

    if len(sys.argv) != 2:
        usage()

    infile = sys.argv[1]

    f = file(infile)
    contents = f.read()
    f.close()

    s_program = '\n'.join(ss_to_s(contents))

    print s_program

    if infile.endswith('.ss'):
        outfile = infile[:-1]
    else:
        outfile = infile + '.s'

    f = file(outfile, 'w')
    f.write(s_program)
    f.close()

