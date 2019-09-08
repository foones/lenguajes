#!/usr/bin/python2
#
# Copyright (C) 2012 Pablo Barenbaum <foones@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#

import re
import sys
import string
import readline

PROMPT = 'Cratylus'
OPTIONS = {
    'verbose': False,
    'script': False,
    'modulo': 0,
    'allow_maximal_powers': False,
}

MXL_POWER = '@'
INPUT_VAR = '<'
OUTPUT_VAR = '>'

class CratylusException(Exception):

    def __init__(self, msg, pos=None):
        self._msg = msg
        self._pos = pos

    def __str__(self):
        if self._pos == None:
            return self._msg
        else:
            return 'at %s\n%s' % (self._pos, indent(self._msg))

def is_numeric(x):
    for c in x:
        if c not in '0123456789':
            return False
    return len(x) > 0

def is_character_literal(x):
    return len(x) == 2 and x[0] == "'"

def is_character_code(x):
    # Allow 256 for representing EOF
    return is_numeric(x) and 0 <= int(x) <= 256

def io_input_char():
    if OPTIONS['script']:
        rd = sys.stdin.read(1)
        if len(rd) == 0:
            user_input = 256
        else:
            user_input = ord(rd)
    else:
        prompt = "input (format: either <char-code> or '<char>) ? "
        user_input = raw_input(prompt)
        while not is_character_code(user_input) and not is_character_literal(user_input):
            sys.stderr.write("!!! wrong input: format should be either <char-code> or '<char>\n")
            user_input = raw_input(prompt)
        if is_character_literal(user_input):
            user_input = ord(user_input[1])
        else:
            user_input = int(user_input)
    return user_input

def io_output_char(x):
    sys.stdout.write(chr(x % 256))
    sys.stdout.flush()

def log(msg):
    sys.stderr.write('%s\n' % (msg,))

def normalize_modulo(x, modulo=0):
    if modulo == 0:
        return x
    else: 
        return x % modulo

def normalize_key(k):
    def lower(v):
        if v.lower() == v:
            return 0
        else:
            return 1
    return tuple(sorted([(v, p) for (v, p) in k if p != 0], key=lambda (v, p): (lower(v), v)))

def cmp_keys(k1, k2):
    all_vars = {}
    for v, p in list(k1) + list(k2):
        all_vars[v] = 1
    k1 = dict(k1)
    k2 = dict(k2)
    all_vars = sorted(all_vars.keys())
    k1 = [k1.get(v, 0) for v in all_vars]
    k2 = [k2.get(v, 0) for v in all_vars]
    return cmp(k1, k2)

def lexisorted(keys):
    return list(reversed(sorted(keys, cmp=cmp_keys)))

def add_powers(p1, p2):
    if p1 == MXL_POWER or p2 == MXL_POWER:
        return MXL_POWER
    else:
        return p1 + p2

def mul_keys(k1, k2):
    vp = {}
    for v, p in list(k1) + list(k2):
        vp[v] = add_powers(vp.get(v, 0), p)
    return normalize_key(vp.items())

def poly_from_constant(k, modulo=0):
    if k == 0:
        return Poly({}, modulo=modulo)
    else:
        return Poly({(): k}, modulo=modulo)

def poly_from_var(var, power=1, modulo=0):
    return Poly({((var, power),): 1}, modulo=modulo)

def poly_from_coeffs(coeffs, var='x', modulo=0):
    dic = {}
    power = 0
    for c in reversed(coeffs):
        dic[((var, power),)] = c
        power += 1
    return Poly(dic, modulo=modulo)

class Poly(object):

    # coeffs is a mapping
    # ((var1, pow1), ..., (varN, powN)) -> coeff
    def __init__(self, coeffs, modulo=0):
        self._modulo = modulo
        self._coeffs = {}
        for k, v in coeffs.items():
            v = normalize_modulo(v, self._modulo)
            if v == 0: continue
            nk = normalize_key(k)
            assert nk not in self._coeffs
            self._coeffs[nk] = v

    def __add__(self, q):
        coeffs = {}
        for k, v in self._coeffs.items() + q._coeffs.items():
            coeffs[k] = coeffs.get(k, 0) + v
        return Poly(coeffs, modulo=self._modulo)

    def __mul__(self, q):
        coeffs = {}
        for k1, v1 in self._coeffs.items():
            for k2, v2 in q._coeffs.items():
                k = mul_keys(k1, k2)
                coeffs[k] = coeffs.get(k, 0) + v1 * v2
        return Poly(coeffs, modulo=self._modulo)

    def __pow__(self, pw):
        if pw == MXL_POWER:
            return self.maximal_power()
        res = poly_from_constant(1, modulo=self._modulo)
        acc = self
        while pw > 0:
            if pw % 2 == 1:
                res = res * acc 
            acc = acc * acc
            pw /= 2
        return res

    def maximal_power(self):
        if not self.is_monomial():
            raise CratylusException('maximal power is allowed for monomials only')
        coeffs = {}
        for k in self._coeffs.keys():
            new_k = tuple([(var, MXL_POWER) for var, power in k])
            coeffs[tuple(new_k)] = self._coeffs[k]
        return Poly(coeffs, modulo=self._modulo)

    def vars_with_maximal_power(self):
        vs = []
        for k in self._coeffs.keys():
            for var, power in k:
                if power == MXL_POWER:
                    vs.append(var)
        return vs

    def has_maximal_power(self):
        return self.vars_with_maximal_power() != []

    def __eq__(self, q):
        ps = sorted(self._coeffs.items())
        qs = sorted(q._coeffs.items())
        return ps == qs
        
    def __sub__(self, q):
        return self + (-q)

    def __neg__(self):
        coeffs = {}
        for k, v in self._coeffs.items():
            coeffs[k] = -v
        return Poly(coeffs, modulo=self._modulo)

    def div_mod(self, d):
        p = self
        q = poly_from_constant(0, modulo=self._modulo)
        r = poly_from_constant(0, modulo=self._modulo)
        d_leading = d.leading_monomial()
        while not p.is_null():
            p_leading = p.leading_monomial()
            qd = d_leading.monomial_div(p_leading)
            if qd is not None:
                q = q + qd
                p = p - qd * d
            else:
                r = r + p_leading
                p = p - p_leading
        assert q * d + r == self
        return q, r

    def rewrite_output(self):
        if not self.is_monomial():
            raise CratylusException('goal in a Cratylus^@ program should be in monomial form: %s' % (self,))

        goal_key, goal_coef = self._coeffs.items()[0]
        if goal_coef != 1:
            raise CratylusException('goal in a Cratylus^@ program should be in monomial form: %s' % (self,))

        goal_key = dict(goal_key)
        result = {}
        for var, power in goal_key.items():
            if power == MXL_POWER:
                raise CratylusException('goal in a Cratylus^@ program should have no maximal powers: %s' % (self,))
            if var == OUTPUT_VAR: 
                io_output_char(power)
            else:
                result[var] = power
        return Poly({tuple(result.items()): 1})

    def rewrite_maximal(self, head, rule_clauses):
        if not self.is_monomial():
            raise CratylusException('goal in a Cratylus^@ program should be in monomial form: %s' % (self,))

        if not head.is_monomial():
            raise CratylusException('rule head in a Cratylus^@ program should be in monomial form: %s' % (head,))

        goal_key, goal_coef = self._coeffs.items()[0]
        if goal_coef != 1:
            raise CratylusException('goal in a Cratylus^@ program should be in monomial form: %s' % (self,))

        head_key, head_coef = head._coeffs.items()[0]
        if head_coef != 1:
            raise CratylusException('rule head in a Cratylus^@ program should be in monomial form: %s' % (head,))

        goal_key = dict(goal_key)
        head_key = dict(head_key)

        rule_reads_input = False

        mxl_power = None
        for var, power in head_key.items():

            if var == OUTPUT_VAR:
                raise CratylusException('output in a Cratylus^@ program not allowed in rule head: %s' % (head,))

            if var == INPUT_VAR:
                if power != MXL_POWER:
                    raise CratylusException('input in a Cratylus^@ program should have maximal power: %s' % (head,))
                rule_reads_input = True
                continue

            goal_power = goal_key.get(var, 0)
            if power == MXL_POWER:
                if mxl_power is None or goal_power < mxl_power:
                    mxl_power = goal_power
                power = 1
            if goal_power < power:
                return None

        if rule_reads_input:
            # Input variable power
            goal_power = io_input_char()
            if mxl_power is None or goal_power < mxl_power:
                mxl_power = goal_power

        result = {}
        for var, power in goal_key.items():

            if var == INPUT_VAR:
                raise CratylusException('input in a Cratylus^@ program not allowed in rule goal: %s' % (self,))

            head_power = head_key.get(var, 0)
            if head_power == MXL_POWER:
                head_power = mxl_power
            result[var] = power - head_power

        for clause in rule_clauses:
            if not clause.is_monomial():
                raise CratylusException('rule body in a Cratylus^@ program should be in monomial form: %s' % (clause,))

            clause_key, clause_coef = clause._coeffs.items()[0]
            if clause_coef != 1:
                raise CratylusException('rule body in a Cratylus^@ program should be in monomial form: %s' % (clause,))

            clause_key = dict(clause_key)
            for var, power in clause_key.items():
                if power == MXL_POWER:
                    power = mxl_power
                result[var] = result.get(var, 0) + power

        return Poly({tuple(result.items()): 1})

    def __div__(self, p):
        q, r = self.div_mod(p)
        return q

    def __mod__(self, p):
        q, r = self.div_mod(p)
        return r

    def leading_monomial(self):
        k = lexisorted(self._coeffs.keys())[0]
        return Poly({k: self._coeffs[k]}, modulo=self._modulo)

    def coefficients(self):
        return self._coeffs

    def as_constant(self):
        if len(self._coeffs) == 0:
            return 0
        elif len(self._coeffs) == 1 and self._coeffs.keys()[0] == ():
            return self._coeffs.values()[0]
        else:
            return None

    def is_monomial(self):
        return len(self._coeffs) == 1

    def is_univariate(self, var):
        for k, v in self._coeffs.items():
            if len(k) == 0:
                continue
            if len(k) > 1:
                return False
            kvar, kpow = k[0]
            if kvar != var:
                return False
        return True

    def is_null(self):
        return len(self._coeffs) == 0

    def monomial_div(self, monomial):
        assert self.is_monomial()
        assert monomial.is_monomial()
        k1, c1 = self._coeffs.items()[0]
        k2, c2 = monomial._coeffs.items()[0]
        if c2 % c1 != 0:
            return None

        k1 = dict(k1)
        k2 = dict(k2)
        res = []

        for var, pow1 in k1.items(): 
            pow2 = k2.get(var, 0)
            if pow2 < pow1:
                return None

        for var, pow2 in k2.items(): 
            pow1 = k1.get(var, 0)
            res.append((var, pow2 - pow1))

        res = Poly({tuple(res): c2 / c1}, modulo=self._modulo)
        assert self * res == monomial 
        return res

    def __repr__(self):
        return self.repr_normal()

    def repr_compact(self, compact=True):
        if not self.is_univariate('x') or self._modulo != 2 or not compact:
            return self.repr_normal()

        max_power = 0
        for k in lexisorted(self._coeffs.keys()):
            if k == ():
                power = 0
            else:
                _, power = k[0]
            max_power = max(power, max_power)

        res = ['0' for i in range(max_power + 1)]
        for k in lexisorted(self._coeffs.keys()):
            if k == ():
                power = 0
            else:
                _, power = k[0]
            res[power] = str(self._coeffs[k])
        return '|%s|' % (''.join(reversed(res)),)

    def repr_normal(self):
        res = []
        fst = True
        for k in lexisorted(self._coeffs.keys()):
            coef = self._coeffs[k]

            if coef < 0:
                if fst:
                    s_pre = '-'
                else:
                    s_pre = ' - '
            elif not fst:
                s_pre = ' + '
            else:
                s_pre = ''

            if fst:
                fst = False

            def s_pow(v, p):
                if p == 1:
                    if v[0] in string.uppercase:
                        return '%s ' % (v,)
                    else:
                        return '%s' % (v,)
                else:
                    return '%s^%s' % (v, p)

            s_var = ''.join([s_pow(v, p) for v, p in k]).strip(' ')
            s_var = re.sub(' ([A-Z])', '\\1', s_var)

            if abs(coef) == 1 and s_var != '':
                s_coef = ''
            else:
                s_coef = '%s' % (abs(coef),)

            res.append('%s%s%s' % (s_pre, s_coef, s_var))
        if res == []:
            return '0'
        else:
            return ''.join(res)

def indent(text):
    return '\n'.join(['    ' + t for t in text.split('\n')])

class Position(object): 

    def __init__(self, filename, string, begin, end):
        self._filename = filename
        self._string = string
        self._begin = begin
        self._end = end

    def line(self):
        line = 1
        chunks = self._string.split('\n')
        rd = 0
        for c in chunks:
            rd += len(c) + 1
            if rd >= self._begin:
                break
            line += 1
        return line

    def fragment(self):
        line = self.line() - 1
        chunks = self._string.split('\n')
        lines = chunks[max(0, line - 1):min(len(chunks), line + 1)]
        return '\n'.join(lines)

    def __repr__(self):
        return '\'%s\', line %s:\n%s\n' % (self._filename, self.line(), indent(self.fragment()))

class Token(object):

    def __init__(self, type, value, position):
        self.type = type
        self.value = value
        self.pos = position

    def __repr__(self):
        return '%s "%s"' % (self.type, self.value)

def tokenize(s, filename='...', modulo=0):
    i = 0
    while i < len(s):

        # Eat whitespace
        while i < len(s) and (s[i] in string.whitespace or s[i] == '#'):
            if s[i] == '#':
                while i < len(s) and s[i] != '\n':
                    i += 1
            else:
                i += 1

        if i >= len(s):
            break

        if s[i] in string.digits:
            b = i
            num = ''
            while i < len(s) and s[i] in string.digits: 
                num += s[i]
                i += 1
            yield Token('NUM', int(num), Position(filename, s, b, i))
        elif s[i:i + len(MXL_POWER)] == MXL_POWER:
            if not OPTIONS['allow_maximal_powers']:
                raise CratylusException('maximal powers not allowed')
            yield Token('NUM', MXL_POWER, Position(filename, s, i, i + len(MXL_POWER)))
            i += len(MXL_POWER)
        elif s[i] in string.lowercase:
            yield Token('VAR', s[i], Position(filename, s, i, i + 1))
            i += 1
        elif s[i] == '{':
            b = i
            name = ''
            while i < len(s) and s[i] != '}':
                name += s[i]
                i += 1
            name += '}'
            i += 1
            yield Token('VAR', name, Position(filename, s, b, i))
        elif s[i] in string.uppercase:
            b = i
            name = s[i]
            i += 1
            while i < len(s) and s[i] in '_' + string.lowercase + string.digits:
                name += s[i]
                i += 1
            yield Token('VAR', name, Position(filename, s, b, i))
        elif s[i:i + 1] in [INPUT_VAR, OUTPUT_VAR]:
            if not OPTIONS['allow_maximal_powers']:
                raise CratylusException('input/output extension not enabled without maximal powers')
            name = s[i]
            yield Token('VAR', name, Position(filename, s, i, i + 1))
            i += 1
        elif s[i] == '|' and modulo == 2:
            b = i
            coeffs = []
            i += 1
            while i < len(s) and s[i] in string.digits:
                coeffs.append(int(s[i]))
                i += 1
            if i >= len(s) or s[i] != '|':
                raise CratylusException('Expected "|"', Position(filename, s, b, i))
            i += 1
            yield Token('POLY', poly_from_coeffs(coeffs, modulo=modulo), Position(filename, s, b, i))
        else:
            symbols = {
                '+': 'ADDOP',
                '-': 'ADDOP',
                '*': 'MULOP',
                '/': 'MULOP',
                '%': 'MULOP',
                '^': 'EXPOP',
                '(': 'LPAREN',
                ')': 'RPAREN',
                '?': 'QUERY',
                ',': 'COMMA',
                '.': 'PERIOD',
                '=>': 'THEN',
            }
            for symbol, symbol_type in symbols.items():
                if i + len(symbol) <= len(s) and s[i:i + len(symbol)] == symbol:
                    yield Token(symbol_type, symbol, Position(filename, s, i, i + len(symbol)))
                    i += len(symbol)
                    break
            else:
                raise CratylusException('Unrecognized symbol: %s' % (s[i],), Position(filename, s, i, i))
    yield Token('EOF', '', Position(filename, s, i, i))

def terminators():
    return ['RPAREN', 'THEN', 'COMMA', 'PERIOD', 'EOF']

def parse_num(tokens, i=0):
    if tokens[i].type == 'NUM':
        return i + 1, tokens[i].value
    else:
        raise CratylusException('Parse error: expected a number: %s' % (tokens[i],), tokens[i].pos)

def parse_atom(tokens, i=0, modulo=0):
    if i >= len(tokens):
        raise CratylusException('Parse error', tokens[-1].pos)

    if tokens[i].type == 'VAR':
        return i + 1, poly_from_var(tokens[i].value, modulo=modulo)
    elif tokens[i].type == 'NUM':
        return i + 1, poly_from_constant(tokens[i].value, modulo=modulo)
    elif tokens[i].type == 'POLY':
        return i + 1, tokens[i].value
    elif tokens[i].type == 'LPAREN':
        j, res = parse_polynomial(tokens, i + 1, modulo=modulo)
        if tokens[j].type != 'RPAREN':
            raise CratylusException('Unbalanced paren', tokens[j].pos)
        return j + 1, res 
    else:
        raise CratylusException('Parse error: unexpected token found: %s' % (tokens[i],), tokens[i].pos)

def parse_factor(tokens, i=0, modulo=0):
    res = poly_from_constant(1, modulo=modulo)
    while i < len(tokens) and tokens[i].type not in ['MULOP', 'ADDOP'] + terminators():
        i, a = parse_atom(tokens, i, modulo=modulo)
        while i < len(tokens) and tokens[i].type in ['EXPOP']:
            i, p = parse_num(tokens, i + 1)
            a = a ** p
        res = res * a
    return i, res

def parse_term(tokens, i=0, modulo=0):
    res = poly_from_constant(1, modulo=modulo)
    nextop = '*'
    while i < len(tokens) and tokens[i].type not in ['ADDOP'] + terminators():
        i, a = parse_factor(tokens, i, modulo=modulo)

        if nextop == '*':
            res = res * a
        elif nextop == '/':
            if a.is_null():
                raise CratylusException('Division by zero.', tokens[i].pos)
            res = res / a
        elif nextop == '%':
            if a.is_null():
                raise CratylusException('Division by zero.', tokens[i].pos)
            res = res % a

        if i < len(tokens) and tokens[i].type in ['MULOP']:
            nextop = tokens[i].value
            i += 1
        else:
            nextop = '*'

    return i, res

def parse_polynomial(tokens, i=0, modulo=0):
    res = poly_from_constant(0, modulo=modulo)

    sign = '+'
    if i < len(tokens) and tokens[i].type in 'ADDOP':
        sign = tokens[i].value
        i += 1

    while i < len(tokens):
        i, f = parse_term(tokens, i, modulo=modulo)
        if sign == '-':
            f = -f
        res = res + f
        if tokens[i].type in ['RPAREN'] + terminators():
            break
        if tokens[i].type not in 'ADDOP':
            raise CratylusException('Expected an additive operator (+, -)', tokens[i].pos)
        sign = tokens[i].value
        i += 1
    return i, res

def poly_from_string(string, modulo=0):
    return parse_polynomial(list(tokenize(string, modulo=modulo)), i=0, modulo=modulo)[1]

def parse_clause(tokens, i, modulo=0):
    clause = []
    while i < len(tokens):
        i, poly = parse_polynomial(tokens, i, modulo=modulo)
        clause.append(poly)
        if tokens[i].type == 'COMMA':
            i += 1
            continue 
        elif tokens[i].type == 'PERIOD':
            i += 1
            break
        else:
            raise CratylusException('Parse error: expected "," or "."', tokens[i].pos)
    return i, clause

class Rule(object):

    def __init__(self, head, clause=[]):
        self.head = head
        self.clause = clause

    def __repr__(self):
        return self.repr_compact(compact=False)

    def repr_compact(self, compact=True):
        if self.clause == []:
            return '%s' % (self.head.repr_compact(compact),)
        else:
            return '%s => %s' % (self.head.repr_compact(compact), ', '.join([x.repr_compact(compact) for x in self.clause]))

    def is_goal(self):
        return False

    def has_maximal_power(self):
        return self.head.has_maximal_power()

class Goal(object):

    def __init__(self, clause=[]):
        self.clause = clause

    def __repr__(self):
        return self.repr_compact(compact=False)

    def repr_compact(self, compact=True):
        return '? %s' % (', '.join([x.repr_compact(compact) for x in self.clause]),)

    def is_goal(self):
        return True

class Program(object):

    def __init__(self, rules=[]):
        self.rules = rules

    def __repr__(self):
        return self.repr_compact(compact=False)

    def repr_compact(self, compact=True):
        return '\n'.join(['%s.' % (r.repr_compact(compact),) for r in self.rules])

    def has_maximal_power(self):
        for rule in self.rules:
            if not rule.is_goal() and rule.has_maximal_power():
                return True
        return False

def parse_rule(tokens, i, modulo=0):
    i, head = parse_polynomial(tokens, i, modulo=modulo)
    if tokens[i].type == 'PERIOD':
        return i + 1, Rule(head)
    elif tokens[i].type == 'THEN':
        i += 1
        i, clause = parse_clause(tokens, i, modulo=modulo)
        return i, Rule(head, clause)
    else:
        raise CratylusException('Expected "=>" or "."', tokens[i].pos)

def parse_goal(tokens, i, modulo=0):
    i, clause = parse_clause(tokens, i, modulo=modulo)
    return i, Goal(clause)

def run_goal(rules, goal, modulo=0):
    if goal.has_maximal_power():
        raise CratylusException('goal "%s" cannot have a maximal power' % (goal,))
    p0 = poly_from_constant(0, modulo=modulo)
    while True:
        if OPTIONS['allow_maximal_powers']:
            goal = goal.rewrite_output()
        for rule in rules:
            if OPTIONS['allow_maximal_powers']:
                goal1 = goal.rewrite_maximal(rule.head, rule.clause)

                if goal1 is not None:

                    if OPTIONS['verbose']:
                        log(40 * '-')
                        log('Current goal : %s' % (goal,))
                        log('Applying rule: %s' % (rule,))

                    goal = goal1

                    if OPTIONS['verbose']:
                        log('New goal     : %s' % (goal,))

                    break
            else:
                q, r = goal.div_mod(rule.head)

                if r == p0:

                    if OPTIONS['verbose']:
                        log(40 * '-')
                        log('Current goal : %s' % (goal,))
                        log('Applying rule: %s' % (rule,))
                        log('Factorization: %s = (%s) * (%s)' % (goal, rule.head, q))

                    goal = q
                    for p in rule.clause:
                        goal = goal * p

                    if OPTIONS['verbose']:
                        log('New goal     : %s' % (goal,))

                    break
        else:
            if OPTIONS['verbose']:
                log(40 * '-')
                log('Final result:')
            sys.stderr.write('%s\n' % (goal,))
            break

def parse_program(string, filename='...', modulo=0):

    if filename.endswith('.cr2'):
        if modulo != 2 and not OPTIONS['script']:
            sys.stderr.write('! .cr2 file - forcing coefficients in Z_2\n')
        modulo = 2

    if filename.endswith('.crm'):
        if not OPTIONS['allow_maximal_powers']:
            sys.stderr.write('! .crm file - allowing maximal powers\n')
        OPTIONS['allow_maximal_powers'] = True

    tokens = list(tokenize(string, filename, modulo=modulo))
    rules = []
    i = 0
    while i < len(tokens) and tokens[i].type != 'EOF':
        if tokens[i].type == 'QUERY':
            i += 1
            i, goal = parse_goal(tokens, i, modulo=modulo)
            rules.append(goal)
        else:
            i, rule = parse_rule(tokens, i, modulo=modulo)
            rules.append(rule)
            if not rule.head.has_maximal_power() and any([m.has_maximal_power() for m in rule.clause]):
                raise CratylusException('rule "%s" body can have maximal power only if head does' % (rule,))
    return Program(rules)

def load_program(string, filename='...', modulo=0):
    program = parse_program(string, filename=filename, modulo=modulo)
    rules = []
    for p in program.rules:
        if p.is_goal():
            for goal in p.clause:
                run_goal(rules, goal, modulo=modulo)
        else:
            rules.append(p)
    return rules

def load_program_from_file(filename, modulo=0):
    if not OPTIONS['script']:
        sys.stderr.write('! Loading file "%s"\n' % (filename,))

    try:
        f = file(filename, 'r')
    except IOError:
        raise CratylusException('Cannot open file \'%s\'' % (filename,))
    contents = f.read()
    f.close()
    return load_program(contents, filename, modulo=modulo)

def banner():
    sys.stderr.write(r"""
  ____           _         _           
 / ___|_ __ __ _| |_ _   _| |_   _ ___ 
| |   | '__/ _` | __| | | | | | | / __|
| |___| | | (_| | |_| |_| | | |_| \__ \
 \____|_|  \__,_|\__|\__, |_|\__,_|___/
                     |___/             

Copyright (c) 2012 - Pablo Barenbaum <foones@gmail.com>
""")

def cratylus_help():
    print '    Cratylus is an esolang based on polynomial rewriting.'
    print
    print '    Its terms are multivariate polynomials with integer coefficients:'
    print '        42'
    print '        x'
    print '        2x + xy + 2y'
    print '        3x^2 - 1'
    print
    print '    A Cratylus program is a sequence of rewriting rules:'
    print '        2x => x.'
    print '        xy => x.'
    print
    print '    Once loaded, the prompt asks for an input polynomial and'
    print '    in case the process terminates, it outputs the normal form:'
    print '        ? 12xy^9'
    print '        3x'
    print
    print '    help        displays this message'
    print '    exit        quit the Cratylus interpreter'

def toplevel(rules, modulo=0):
    while True:
        goal_string = raw_input('? ')
        if goal_string in ['bye', 'quit', 'exit']:
            sys.stderr.write('Bye.\n')
            break
        elif goal_string in ['help']:
            cratylus_help()
            continue
        try:
            goal = poly_from_string(goal_string, modulo=modulo)
            run_goal(rules, goal, modulo=modulo)
        except CratylusException, e:
            sys.stderr.write('%s\n' % (e,))
        except KeyboardInterrupt, e:
            pass

def usage(exit=True):
    banner()
    sys.stderr.write('Usage: %s [options] <file>\n' % (sys.argv[0],))
    sys.stderr.write('Options:\n')
    sys.stderr.write('    -v, --verbose          trace every step\n')
    sys.stderr.write('    -s, --script           do not start toplevel interaction\n')
    sys.stderr.write('    -b, --binary           coefficients are in Z_2\n')
    sys.stderr.write('    -m, --maximal          allow maximal powers (x^@)\n')
    if exit:
        sys.exit(1)

if __name__ == '__main__':
    args = []
    i = 1
    while i < len(sys.argv):
        if sys.argv[i] in ['-h', '--help']:
            usage()
        elif sys.argv[i] in ['-v', '--verbose']:
            OPTIONS['verbose'] = True
            i += 1
        elif sys.argv[i] in ['-s', '--script']:
            OPTIONS['script'] = True
            i += 1
        elif sys.argv[i] in ['-b', '--binary']:
            OPTIONS['modulo'] = 2
            i += 1
        elif sys.argv[i] in ['-m', '--maximal']:
            OPTIONS['allow_maximal_powers'] = True
            i += 1
        else:
            args.append(sys.argv[i])
            i += 1

    if len(args) != 1:
        usage(exit=False)
        if not OPTIONS['script']:
            toplevel([], modulo=OPTIONS['modulo'])
        sys.exit(1)

    try:
        if not OPTIONS['script']:
            banner()

        rules = load_program_from_file(args[0], modulo=OPTIONS['modulo'])
        if not OPTIONS['script']:
            toplevel(rules, OPTIONS['modulo'])

    except CratylusException, e:
        sys.stderr.write('%s: %s\n' % (PROMPT, e,))

