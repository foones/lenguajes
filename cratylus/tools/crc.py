#!/usr/bin/python2
import os
import sys
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

import string

import cratylus

OPTIONS = {
    'gmp': True,
}

class CrcException(Exception):
    pass

def collect_polys(program):
    all_polys = []
    for rule in program.rules:
        if rule.is_goal():
            all_polys.extend(rule.clause)
        else:
            all_polys.append(rule.head)
            all_polys.extend(rule.clause)
    return all_polys

def in_monomial_form(polys):
    for poly in polys:
        if not poly.is_monomial():
            return False
    return True

def in_univariate_form(polys):
    for poly in polys:
        if not poly.is_univariate('x'):
            return False
    return True

def cratylus_compile(filename, program):
    program = cratylus.parse_program(program, filename)

    all_polys = collect_polys(program)
    if in_monomial_form(all_polys):
        return crc(program)
    elif in_univariate_form(all_polys):
        return uni_crc(program)
    else:
        raise CrcException('program is not in monomial or univariate form, cannot compile')

#### Monomial form compiler

def crc_collect_constants(polys):
    constants = {}
    for poly in polys:
        key, coef = poly.coefficients().items()[0]
        for var, power in key:
            if power == cratylus.MXL_POWER:
                power = 1
            constants[power] = True
    return constants

def crc_condition(monomial, table, constants_invtable=None):
    key, coef = monomial.coefficients().items()[0]
    cond = []
    for var, power in key:
        if var == cratylus.OUTPUT_VAR:
            raise CrcException('rule head should not have output var: %s' % (monomial,))
        if var == cratylus.INPUT_VAR:
            # Do not check anything here.
            continue
        if power == cratylus.MXL_POWER:
            power = 1
        if OPTIONS['gmp']:
            cond.append('mpz_cmp(v[%u], c[%u]) >= 0' % (table[var], constants_invtable[power]))
        else:
            cond.append('v[%u] >= %u' % (table[var], power))
    if cond == []:
        return '1'
    else:
        return ' && '.join(cond)

def crc_inc_dec_maximal(rule, table, indent=''):
    if not rule.head.has_maximal_power():
        return ''
    stmt = []

    # Variables with maximal power at the left-hand side of the rule
    left_vars = rule.head.vars_with_maximal_power()

    if cratylus.INPUT_VAR in left_vars:
        # Rule reads input
        stmt.append('int r = fgetc(stdin);\n')
        stmt.append('if (r == EOF) {\n')
        stmt.append('\tr = 256;\n')
        stmt.append('}\n')
        if OPTIONS['gmp']:
            stmt.append('mpz_set_ui(v[INVAR], r);\n')
        else:
            stmt.append('v[INVAR] = r;\n')

    # Variables with maximal power at the right-hand side of the rule
    right_vars = {}
    for m in rule.clause:
        for v in m.vars_with_maximal_power():
            right_vars[v] = 1
    right_vars = right_vars.keys()

    if OPTIONS['gmp']:
        stmt.append('mpz_set(mxl, v[%u]);\n' % (table[left_vars[0]],))
        for v in left_vars[1:]:
            stmt.append('if (mpz_cmp(mxl, v[%u]) > 0) {\n' % (table[v],))
            stmt.append('\tmpz_set(mxl, v[%u]);\n' % (table[v],))
            stmt.append('}\n')

        for v in left_vars:
            stmt.append('mpz_sub(v[%u], v[%u], mxl);\n' % (table[v], table[v]))

        for v in right_vars:
            stmt.append('mpz_add(v[%u], v[%u], mxl);\n' % (table[v], table[v]))

    else:
        stmt.append('mxl = v[%u];\n' % (table[left_vars[0]],))
        for v in left_vars[1:]:
            stmt.append('if (mxl > v[%u]) {\n' % (table[v],))
            stmt.append('\tmxl = v[%u];\n' % (table[v],))
            stmt.append('}\n')

        for v in left_vars:
            stmt.append('v[%u] -= mxl;\n' % (table[v],))

        for v in right_vars:
            stmt.append('v[%u] += mxl;\n' % (table[v],))

    return ''.join([indent + s for s in stmt])

def crc_inc_dec(monomial, table, sign, constants_invtable=None, indent=''):
    key, coef = monomial.coefficients().items()[0]
    stmt = []
    for var, power in key:
        if power == cratylus.MXL_POWER:
            continue
        if OPTIONS['gmp']:
            if sign == '+':
                func_sign = 'mpz_add'
            else:
                func_sign = 'mpz_sub'
            stmt.append('%s%s(v[%u], v[%u], c[%u]);\n' % (indent, func_sign, table[var], table[var], constants_invtable[power]))
        else:
            stmt.append('%sv[%u] %s= %u;\n' % (indent, table[var], sign, power))
    return ''.join(stmt)

def crc(program):

    all_polys = collect_polys(program)
    constants_invtable = None

    # Count variables
    var_count = {}
    for poly in all_polys:
        if not poly.is_monomial():
            raise CrcException('"%s" is not a monomial' % (poly,))
        key, coef = poly.coefficients().items()[0]
        if coef != 1:
            raise CrcException('"%s" is not in monomial form (coeff should be 1)' % (poly,))
        for var, power in key:
            if var == cratylus.INPUT_VAR and power != cratylus.MXL_POWER:
                raise CrcException('input variable should have maximal power: %s' % (poly,))
            var_count[var] = var_count.get(var, 0) + 1

    # Build translation table
    num_vars = len(var_count) # number of distinct variables
    old_vars = sorted(var_count.items(), key=lambda (v, c): -c)
    old_vars = [v for v, c in old_vars]
    new_vars = list(range(num_vars))
    table = dict(zip(old_vars, new_vars))

    reverse_table = {}
    for k, v in table.items():
        reverse_table[v] = k
    reverse_table = [k for v, k in sorted(reverse_table.items())]

    prog = []
    prog.append('/* Generated by the Cratylus to C compiler */\n\n')
    if OPTIONS['gmp']:
        prog.append('/* Compile with -lgmp */\n\n')
    prog.append('/*\n')
    for old, new in sorted(table.items()):
        prog.append( '* %s --> %s\n' % (old, new))
    prog.append('*/\n')
    prog.append('\n')
    prog.append('#include <stdio.h>\n')
    if OPTIONS['gmp']:
        prog.append('#include <gmp.h>\n')
    prog.append('\n')

    if OPTIONS['gmp']:
        constants = crc_collect_constants(all_polys)
        
        num_constants = len(constants)
        prog.append('#define CONSTANTS %u\n' % (num_constants,))
        prog.append('mpz_t c[CONSTANTS];\n\n')
        constants = sorted(constants.keys())

        constants_invtable = {}
        i = 0
        for constant in constants:
            constants_invtable[constant] = i
            i += 1

    prog.append('#define VARS %u\n' % (num_vars,))
    if OPTIONS['gmp']:
        prog.append('mpz_t v[VARS];\n')
    else:
        prog.append('unsigned long int v[VARS];\n')
    prog.append('\n')

    if cratylus.INPUT_VAR in table:
        prog.append('#define INVAR %u\n' % (table[cratylus.INPUT_VAR],))
    if cratylus.OUTPUT_VAR in table:
        prog.append('#define OUTVAR %u\n' % (table[cratylus.OUTPUT_VAR],))
    prog.append('\n')

    prog.append('char *n[] = {\n')
    for varname in reverse_table:
        prog.append('\t"%s",\n' % (varname,))
    prog.append('};\n')
    prog.append('\n')

    # Translate program
    prog.append('int main()\n')
    prog.append('{\n')
    prog.append('\tint i, z;\n')
    if program.has_maximal_power():
        if OPTIONS['gmp']:
            prog.append('\tmpz_t mxl;\n')
            prog.append('\tmpz_init(mxl);\n')
        else:
            prog.append('\tunsigned long int mxl;\n')
    prog.append('\n')

    if OPTIONS['gmp']:
        prog.append('\t/* Initialize constants */\n')
        for i in range(num_constants):
            prog.append('\tmpz_init_set_str(c[%u], "%s", 10);\n' % (i, constants[i],))
        prog.append('\n')

    prog.append('\t/* Initialize variables */\n')
    prog.append('\tfor (i = 0; i < VARS; i++) {\n')
    if OPTIONS['gmp']:
        prog.append('\t\tmpz_init(v[i]);\n')
    else:
        prog.append('\t\tv[i] = 0;\n')
    prog.append('\t}\n')
    prog.append('\n')

    ngoals = 0
    for rule in program.rules:
        if not rule.is_goal(): continue
        ngoals += 1
        if ngoals > 1:
            raise CrcException('Cratylus to C compiler supports at most one goal')
        prog.append('\t/* Goal: %s */\n' % (rule,))
        for m in rule.clause:
            prog.append(crc_inc_dec(m, table, '+', constants_invtable, indent='\t'))
        prog.append('\n')

    prog.append('\twhile (1) {\n')
    if cratylus.OUTPUT_VAR in table:
        if OPTIONS['gmp']:
            prog.append('\t\tif (mpz_cmp_ui(v[OUTVAR], 0) > 0) {\n')
            prog.append('\t\t\tint r = mpz_get_ui(v[OUTVAR]);\n')
            prog.append('\t\t\tprintf("%c", (char)r);\n')
            prog.append('\t\t\tfflush(stdout);\n')
            prog.append('\t\t\tmpz_set_ui(v[OUTVAR], 0);\n')
            prog.append('\t\t}\n')
        else:
            prog.append('\t\tif (v[OUTVAR] > 0) {\n')
            prog.append('\t\t\tint r = v[OUTVAR];\n')
            prog.append('\t\t\tprintf("%c", (char)r);\n')
            prog.append('\t\t\tfflush(stdout);\n')
            prog.append('\t\t\tv[OUTVAR] = 0;\n')
            prog.append('\t\t}\n')

    prog.append('\t\tif (0) {\n')
    for rule in program.rules:
        if rule.is_goal(): continue
        prog.append('\t\t} else if (%s) {\n' % (crc_condition(rule.head, table, constants_invtable),))
        prog.append('\t\t\t/* %s */\n' % (rule,))
        prog.append(crc_inc_dec_maximal(rule, table, indent='\t\t\t'))
        prog.append(crc_inc_dec(rule.head, table, '-', constants_invtable, indent='\t\t\t'))
        for m in rule.clause:
            prog.append(crc_inc_dec(m, table, '+', constants_invtable, indent='\t\t\t'))
    prog.append('\t\t} else {\n')
    prog.append('\t\t\tbreak;\n')
    prog.append('\t\t}\n')
    prog.append('\t}\n')
    prog.append('\n')
    prog.append('\tz = 1;\n')
    prog.append('\tfor (i = 0; i < VARS; i++) {\n')
    if OPTIONS['gmp']:
        prog.append('\t\tif (mpz_cmp_ui(v[i], 0) > 0) {\n')
    else:
        prog.append('\t\tif (v[i] > 0) {\n')
    prog.append('\t\t\tif (!z) {\n')
    prog.append('\t\t\t\tfprintf(stderr, " ");\n')
    prog.append('\t\t\t}\n')
    prog.append('\t\t\tz = 0;\n')
    prog.append('\t\t\tfprintf(stderr, "%s", n[i]);\n')
    if OPTIONS['gmp']:
        prog.append('\t\t\tif (mpz_cmp_ui(v[i], 1) > 0) {\n')
    else:
        prog.append('\t\t\tif (v[i] > 1) {\n')
    prog.append('\t\t\t\tfprintf(stderr, "^");\n')
    if OPTIONS['gmp']:
        prog.append('\t\t\t\tmpz_out_str(stderr, 10, v[i]);\n')
    else:
        prog.append('\t\t\t\tfprintf(stderr, "%lu", v[i]);\n')
    prog.append('\t\t\t}\n')
    prog.append('\t\t}\n')
    prog.append('\t}\n')
    prog.append('\tif (z) {\n')
    prog.append('\t\tfprintf(stderr, "1");\n')
    prog.append('\t}\n')
    prog.append('\tfprintf(stderr, "\\n");\n')
    prog.append('\n')
    prog.append('\treturn 0;\n')
    prog.append('}\n')
    return ''.join(prog)

#### Univariate form compiler

def uni_serialize_poly(poly):
    coef_by_deg = {}
    for k, coef in poly.coefficients().items():
        if k == ():
            coef_by_deg[0] = coef
        elif len(k) == 1:
            var, power = k[0]
            if var != 'x':
                raise CrcException('polynomial %s is not univariate in x' % (poly,))
            coef_by_deg[power] = coef
        else:
            raise CrcException('polynomial %s is not univariate' % (poly,))
    
    degree = 0
    for d, coef in coef_by_deg.items():
        degree = max(degree, d)

    serial = []
    serial.append(degree)
    serial.extend([coef_by_deg.get(d, 0) for d in range(degree + 1)])
    return '/* %s */ {%s}' % (poly, ', '.join([str(s) for s in serial]))

def uni_crc(program):
    prog = []
    prog.append('#include <stdio.h>\n')
    prog.append('#include <stdlib.h>\n')
    prog.append('\n')
    prog.append('typedef signed long int C;\n')
    prog.append('\n')
    prog.append('typedef struct {\n')
    prog.append('\tC *h;\n')
    prog.append('\tC *b;\n')
    prog.append('} R;\n')
    prog.append('\n')
    prog.append('#define NPOLYS 8\n')
    prog.append('signed long int limdeg;\n')
    prog.append('C *p[NPOLYS];\n')
    prog.append('\n')

    num_rules = 0
    for rule in program.rules:
        if rule.is_goal(): continue
        num_rules += 1

    prog.append('#define NRULES %u\n' % (num_rules,))
    prog.append('R r[NRULES];\n')
    prog.append('\n')

    rule_index = 0
    for rule in program.rules:
        if rule.is_goal(): continue
        prog.append('C h%u[] = %s;\n' % (rule_index, uni_serialize_poly(rule.head),))

        if len(rule.clause) > 1:
            raise CrcException('rule %s should have at most one clause in the body' % (rule,))

        if len(rule.clause) == 0:
            prog.append('C b%u[] = /* 1 */ {0, 1};\n' % (rule_index,))
        else:
            for m in rule.clause:
                prog.append('C b%u[] = %s;\n' % (rule_index, uni_serialize_poly(m),))

        rule_index += 1

    ngoals = 0
    for rule in program.rules:
        if not rule.is_goal(): continue
        ngoals += 1
        if ngoals > 1:
            raise CrcException('Cratylus to C compiler supports at most one goal')
        if len(rule.clause) > 1:
            raise CrcException('goal %s should have at most one clause' % (rule,))
        for m in rule.clause:
            prog.append('C g[] = %s;\n' % (uni_serialize_poly(m),))

    prog.append('\n')
    prog.append('#define deg(P)            ((P)[0])\n')
    prog.append('#define coef(P, I)        ((I) <= deg(P) ? (P)[(I) + 1] : 0)\n')
    prog.append('#define setcoef(P, I, X)  ((P)[(I) + 1]) = (X)\n')
    prog.append('#define ABS(X)            ((X) < 0 ? -(X) : (X))\n')
    prog.append('#define MAX(X, Y)         ((X) > (Y) ? (X) : (Y))\n')
    prog.append('\n')
    prog.append('void show_poly(C *poly)\n')
    prog.append('{\n')
    prog.append('\tint i;\n')
    prog.append('\tC ci;\n')
    prog.append('\tint z = 1;\n')
    prog.append('\tint fst = 1;\n')
    prog.append('\tfor (i = deg(poly); i >= 0; i--) {\n')
    prog.append('\t\tci = coef(poly, i);\n')
    prog.append('\t\tif (ci != 0) {\n')
    prog.append('\t\t\tz = 0;\n')
    prog.append('\t\t\tif (ci < 0 && fst) {\n')
    prog.append('\t\t\t\tfprintf(stderr, "-");\n')
    prog.append('\t\t\t}\n')
    prog.append('\t\t\tif (!fst) {\n')
    prog.append('\t\t\t\tif (ci < 0) {\n')
    prog.append('\t\t\t\t\tfprintf(stderr, " - ");\n')
    prog.append('\t\t\t\t} else {\n')
    prog.append('\t\t\t\t\tfprintf(stderr, " + ");\n')
    prog.append('\t\t\t\t}\n')
    prog.append('\t\t\t}\n')
    prog.append('\t\t\tfst = 0;\n')
    prog.append('\t\t\tif (i == 0 || ABS(ci) != 1) {\n')
    prog.append('\t\t\t\tfprintf(stderr, "%li", ABS(ci));\n')
    prog.append('\t\t\t}\n')
    prog.append('\t\t\tif (i == 1) {\n')
    prog.append('\t\t\t\tfprintf(stderr, "x");\n')
    prog.append('\t\t\t} else if (i > 1) {\n')
    prog.append('\t\t\t\tfprintf(stderr, "x^%u", i);\n')
    prog.append('\t\t\t}\n')
    prog.append('\t\t}\n')
    prog.append('\t}\n')
    prog.append('\tif (z) {\n')
    prog.append('\t\tfprintf(stderr, "0");\n')
    prog.append('\t}\n')
    prog.append('\tfprintf(stderr, "\\n");\n')
    prog.append('}\n')
    prog.append('\n')
    prog.append('void copy_poly(C *dst, C *src)\n')
    prog.append('{\n')
    prog.append('\tint i;\n')
    prog.append('\tdeg(dst) = deg(src);\n')
    prog.append('\tfor (i = 0; i <= deg(src); i++) {\n')
    prog.append('\t\tsetcoef(dst, i, coef(src, i));\n')
    prog.append('\t}\n')
    prog.append('}\n')
    prog.append('\n')
    prog.append('void ensure_deg(int d)\n')
    prog.append('{\n')
    prog.append('\tint i, j;\n')
    prog.append('\tC *newp, *oldp;\n')
    prog.append('\tint newlim = limdeg;\n')
    prog.append('\twhile (d + 3 >= newlim) {\n')
    prog.append('\t\tnewlim = 2 * newlim;\n')
    prog.append('\t}\n')
    prog.append('\tif (newlim == limdeg) return;\n')
    prog.append('\t\n')
    prog.append('\tfor (i = 0; i < NPOLYS; i++) {\n')
    prog.append('\t\tnewp = (C *)malloc(sizeof(C) * newlim);\n')
    prog.append('\t\tfor (j = 0; j < limdeg; j++) {\n')
    prog.append('\t\t\tnewp[j] = p[i][j];\n')
    prog.append('\t\t}\n')
    prog.append('\t\tfor (j = limdeg; j < newlim; j++) {\n')
    prog.append('\t\t\tnewp[j] = 0;\n')
    prog.append('\t\t}\n')
    prog.append('\t\toldp = p[i];\n')
    prog.append('\t\tp[i] = newp;\n')
    prog.append('\t\tfree(oldp);\n')
    prog.append('\t}\n')
    prog.append('\tlimdeg = newlim;\n')
    prog.append('}\n')
    prog.append('\n')
    prog.append('void zero_poly(C *p, int m)\n')
    prog.append('{\n')
    prog.append('\tint i;\n')
    prog.append('\tdeg(p) = 0;\n')
    prog.append('\tfor (i = 0; i <= m; i++) {\n')
    prog.append('\t\tsetcoef(p, i, 0);\n')
    prog.append('\t}\n')
    prog.append('}\n')
    prog.append('\n')
    prog.append('void fixdeg(C *p)\n')
    prog.append('{\n')
    prog.append('\twhile (deg(p) > 0 && coef(p, deg(p)) == 0) {\n')
    prog.append('\t\tdeg(p)--;\n')
    prog.append('\t}\n')
    prog.append('}\n')
    prog.append('\n')
    prog.append('void addcoef(C *p, int pow, C val)\n')
    prog.append('{\n')
    prog.append('\tsetcoef(p, pow, coef(p, pow) + val);\n')
    prog.append('\tdeg(p) = MAX(deg(p), pow);\n')
    prog.append('\tfixdeg(p);\n')
    prog.append('}\n')
    prog.append('\n')
    prog.append('void add_poly(int x1, int x2, int x3)\n')
    prog.append('{\n')
    prog.append('\tint i;\n')
    prog.append('\tensure_deg(MAX(deg(p[x1]), deg(p[x2])));\n')
    prog.append('\tzero_poly(p[x3], MAX(deg(p[x1]), deg(p[x2])));\n')
    prog.append('\tdeg(p[x3]) = MAX(deg(p[x1]), deg(p[x2]));\n')
    prog.append('\tfor (i = 0; i <= MAX(deg(p[x1]), deg(p[x2])); i++) {\n')
    prog.append('\t\tsetcoef(p[x3], i, coef(p[x1], i) + coef(p[x2], i));\n')
    prog.append('\t}\n')
    prog.append('\tfixdeg(p[x3]);\n')
    prog.append('}\n')
    prog.append('\n')
    prog.append('void mul_poly(int x1, int x2, int x3)\n')
    prog.append('{\n')
    prog.append('\tint i, j;\n')
    prog.append('\tensure_deg(deg(p[x1]) + deg(p[x2]));\n')
    prog.append('\tzero_poly(p[x3], deg(p[x1]) + deg(p[x2]));\n')
    prog.append('\tdeg(p[x3]) = deg(p[x1]) + deg(p[x2]);\n')
    prog.append('\tfor (i = 0; i <= deg(p[x1]); i++) {\n')
    prog.append('\t\tfor (j = 0; j <= deg(p[x2]); j++) {\n')
    prog.append('\t\t\tsetcoef(p[x3], i + j, coef(p[x3], i + j) + coef(p[x1], i) * coef(p[x2], j));\n')
    prog.append('\t\t}\n')
    prog.append('\t}\n')
    prog.append('\tfixdeg(p[x3]);\n')
    prog.append('}\n')
    prog.append('\n')
    prog.append('#define poly_null(P) (deg(P) == 0 && coef((P), 0) == 0)\n')
    prog.append('\n')
    prog.append('void divmod_poly(int xa, int xb, int xq, int xr, int xt1, int xt2)\n')
    prog.append('{\n')
    prog.append('\tint da, db;\n')
    prog.append('\tC lead_a, lead_b, quot;\n')
    prog.append('\t\n')
    prog.append('\tzero_poly(p[xq], MAX(deg(p[xa]), deg(p[xb])));\n')
    prog.append('\tzero_poly(p[xr], MAX(deg(p[xa]), deg(p[xb])));\n')
    prog.append('\twhile (!poly_null(p[xa])) {\n')
    prog.append('\t\tda = deg(p[xa]);\n')
    prog.append('\t\tdb = deg(p[xb]);\n')
    prog.append('\t\tlead_a = coef(p[xa], da);\n')
    prog.append('\t\tlead_b = coef(p[xb], db);\n')
    prog.append('\t\tif (da >= db && lead_a % lead_b == 0) {\n')
    prog.append('\t\t\tquot = lead_a / lead_b;\n')
    prog.append('\t\t\taddcoef(p[xq], da - db, quot);\n')
    prog.append('\t\t\tzero_poly(p[xt1], da);\n')
    prog.append('\t\t\taddcoef(p[xt1], da - db, -quot);\n')
    prog.append('\t\t\tmul_poly(xb, xt1, xt2);\n')
    prog.append('\t\t\tadd_poly(xa, xt2, xt1);\n')
    prog.append('\t\t\tcopy_poly(p[xa], p[xt1]);\n')
    prog.append('\t\t} else {\n')
    prog.append('\t\t\taddcoef(p[xr], da, lead_a);\n')
    prog.append('\t\t\taddcoef(p[xa], da, -lead_a);\n')
    prog.append('\t\t}\n')
    prog.append('\t}\n')
    prog.append('}\n')
    
    prog.append('\n')
    prog.append('int main()\n')
    prog.append('{\n')
    prog.append('\tint i, j, nf;\n')
    for rule_index in range(num_rules):
        prog.append('\tr[%u].h = h%u;\n' % (rule_index, rule_index))
        prog.append('\tr[%u].b = b%u;\n' % (rule_index, rule_index))
        prog.append('\t\n')

    prog.append('\tlimdeg = deg(g);\n')
    prog.append('\tfor (i = 0; i < NRULES; i++) {\n')
    prog.append('\t\tlimdeg = MAX(limdeg, deg(r[i].h));\n')
    prog.append('\t\tlimdeg = MAX(limdeg, deg(r[i].b));\n')
    prog.append('\t}\n')
    prog.append('\tlimdeg += 2;\n')
    prog.append('\t\n')
    prog.append('\tfor (i = 0; i < NPOLYS; i++) {\n')
    prog.append('\t\tp[i] = (C *)malloc(sizeof(C) * limdeg);\n')
    prog.append('\t\tfor (j = 0; j < limdeg; j++) {\n')
    prog.append('\t\t\tp[i][j] = 0;\n')
    prog.append('\t\t}\n')
    prog.append('\t}\n')
    prog.append('\t\n')
    prog.append('\tcopy_poly(p[6], g);\n')
    prog.append('\tnf = 0;\n')
    prog.append('\twhile (!nf) {\n')
    prog.append('\t\tshow_poly(p[6]);\n')
    prog.append('\t\tnf = 1;\n')
    prog.append('\t\tfor (i = 0; i < NRULES; i++) {\n')
    prog.append('\t\t\tcopy_poly(p[0], p[6]);\n')
    prog.append('\t\t\tcopy_poly(p[1], r[i].h);\n')
    prog.append('\t\t\tdivmod_poly(0, 1, 2, 3, 4, 5);\n')
    prog.append('\t\t\tif (poly_null(p[3])) {\n')
    prog.append('\t\t\t\tcopy_poly(p[1], r[i].b);\n')
    prog.append('\t\t\t\tmul_poly(1, 2, 6);\n')
    prog.append('\t\t\t\tnf = 0;\n')
    prog.append('\t\t\t\tbreak;\n')
    prog.append('\t\t\t}\n')
    prog.append('\t\t}\n')
    prog.append('\t}\n')
    prog.append('\t\n')
    prog.append('\tshow_poly(p[6]);\n')
    prog.append('\t\n')
    prog.append('\treturn 0;\n')
    prog.append('}\n')
    return ''.join(prog)

def usage():
    sys.stderr.write('Cratylus to C compiler.\n')
    sys.stderr.write('Copyright (c) 2012 - Pablo Barenbaum <foones@gmail.com>\n')
    sys.stderr.write('Usage:\n')
    sys.stderr.write('    %s <infile.cr> [options]\n' % (sys.argv[0],))
    sys.stderr.write('Options:\n')
    sys.stderr.write('    -no-gmp           do not use GNU Mutiprecision Library\n')
    sys.exit(1)

if __name__ == '__main__':

    outfile = None

    args = []
    i = 1
    while i < len(sys.argv):
        if sys.argv[i] == '-no-gmp':
            OPTIONS['gmp'] = False
        else:
            args.append(sys.argv[i])
        i += 1

    if len(args) != 1:
        usage()

    infile = args[0]

    f = file(infile)
    contents = f.read()
    f.close()

    result = cratylus_compile(infile, contents)

    sys.stderr.write(result)

    if infile.endswith('.cr'):
        outfile = infile[:-1]
    elif infile.endswith('.crm'):
        outfile = infile[:-2]
    else:
        outfile = infile + '.c'
    f = file(outfile, 'w')
    f.write(result)
    f.close()

