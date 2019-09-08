#!/usr/bin/python2
import os
import sys
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

import string
import random

import cratylus

OPTIONS = {
  'source_modulo': 0,
  'target_modulo': 0,
  'compact': True,
}

class SimpCrException(Exception):
    pass

def translate(filename, translation, contents, options):
    if translation == 'normalize':
        return normalize_program(filename, contents)
    else:
        return translate_program(filename, contents, translation, options)

def normalize_program(filename, program):
    return repr(cratylus.parse_program(program, filename, modulo=OPTIONS['source_modulo']))

########################################

def is_prime(p):
    for d in range(2, p):
        if d * d > p:
            break
        if p % d == 0:
            return False
    return True

def gen_primes(n):
    primes = []
    p = 2
    while len(primes) < n:
        if is_prime(p):
            primes.append(p)
        p += 1
    return primes

########################################

def gen_irreducible_Z_polys(n):
    polys = [] 
    for i in range(n):
        k = i // 2 + 1
        if i % 2 == 1:
            k = -k
        polys.append(cratylus.poly_from_var('x', modulo=0) + cratylus.poly_from_constant(k, modulo=0))
    return polys

########################################

def gen_all_Zk_coefs_of_length(length, k):
    if length == 0:
        yield []
    else:
        for c in range(k):
            for cs in gen_all_Zk_coefs_of_length(length - 1, k):
                yield [c] + cs

def gen_all_Zk_coefs(k, from_length=0):
    length = from_length
    while True:
        for cs in gen_all_Zk_coefs_of_length(length, k):
            if cs[-1] == 0:
                continue
            yield cs
        length += 1

def gen_irreducible_Zk_polys(n, k):
    polys = []
    for cs in gen_all_Zk_coefs(k=2, from_length=1):
        if len(polys) == n:
            break
        p = cratylus.poly_from_coeffs([1] + cs, modulo=k)
        for q in polys:
            if (p % q).is_null():
                break
        else:
            polys.append(p)
    return polys

########################################

def alphanumeric_strings_of_length(length):
    if length == 0:
        yield ''
    else:
        for x in string.lowercase:
            for xs in alphanumeric_strings_of_length(length - 1):
                yield x + xs

def alphanumeric_identifiers():
    for x in alphanumeric_strings_of_length(1):
        yield x
    k = 0
    while True:
        k += 1
        for x in alphanumeric_strings_of_length(k):
            yield x[0].upper() + x[1:]

def gen_compact_vars(n):
    res = []
    i = 0
    for x in alphanumeric_identifiers():
        if i == n: break
        res.append(cratylus.poly_from_var(x, modulo=0))
        i += 1
    return res

def gen_words(n):
    fn = os.path.join(os.path.dirname(__file__), 'words.txt')
    f = file(fn, 'r')
    ls = f.readlines()
    f.close()
    random.shuffle(ls)
    for i in range(n):
        yield cratylus.poly_from_var(ls[i].strip(' \t\r\n'), modulo=0)

def gen_mixed(n):
    cons = [gen_words, gen_compact_vars]
    gens = [list(g(n)) for g in cons]
    i = 0
    while i < n:
        yield gens[0].pop(0)
        gens.append(gens.pop(0))
        i += 1

########################################

def irreducible_elements(n, translation_type):
    "Return n irreducible elements appropiate for the given translation type"
    if translation_type == 'fractran':
        return [cratylus.poly_from_constant(p, modulo=0) for p in gen_primes(n)]
    elif translation_type == 'univariate':
        return gen_irreducible_Z_polys(n)
    elif translation_type == 'binary':
        return gen_irreducible_Zk_polys(n, k=2)
    elif translation_type == 'vars_compact':
        return gen_compact_vars(n)
    elif translation_type == 'words':
        return gen_words(n)
    elif translation_type == 'mixed':
        return gen_mixed(n)
    else:
        assert False

def translate_monomial(table, monomial, options):
    key, coef = monomial.coefficients().items()[0]
    res = cratylus.poly_from_constant(1, OPTIONS['target_modulo'])
    for var, power in key:
        if var in table:
            res = res * table[var] ** power
        else:
            assert var in options['initial_table']
            res = res * options['initial_table'][var] ** power
    return res

def add_prefix(mono, prefix):
    if prefix is None:  
        return mono
    prefix = prefix[0].upper() + prefix[1:]

    coeffs = {}
    for key, cf in mono.coefficients().items():
        new_key = []
        for var, power in key:
            if var.startswith('{') and var.endswith('}'):
                new_var = '{' + prefix + '_' + var[1:]
            elif len(var) == 1 and var.lower() == var:
                new_var = prefix + '_' + var.lower()
            else:
                new_var = prefix + '__' + var.lower()
            new_key.append((new_var, power))
        coeffs[tuple(new_key)] = cf
    return cratylus.Poly(coeffs)

def translate_program(filename, program, translation_type, options):
    program = cratylus.parse_program(program, filename, modulo=OPTIONS['source_modulo'])

    # Collect all polynomials
    all_polys = []
    for rule in program.rules:
        if rule.is_goal():
            all_polys.extend(rule.clause)
        else:
            all_polys.append(rule.head)
            all_polys.extend(rule.clause)

    # Count variables
    var_count = {}
    for poly in all_polys:
        if not poly.is_monomial():
            raise SimpCrException('"%s" is not a monomial' % (poly,))
        key, coef = poly.coefficients().items()[0]
        if coef != 1:
            raise SimpCrException('"%s" is not in monomial form (coeff should be 1)' % (poly,))
        for var, power in key:
            if var not in options['initial_table']:
                var_count[var] = var_count.get(var, 0) + 1

    # Build translation table
    num_vars = len(var_count) # number of distinct variables
    old_vars = sorted(var_count.items(), key=lambda (v, c): -c)
    old_vars = [v for v, c in old_vars]
    new_vars = irreducible_elements(num_vars, translation_type)
    new_vars = [add_prefix(v, options['prefix']) for v in new_vars]
    table = dict(zip(old_vars, new_vars))

    comment = []
    for old, new in sorted(table.items() + options['initial_table'].items()):
        msg = '# %s --> %s' % (old, new.repr_compact(OPTIONS['compact'])) 
        comment.append(msg)

    # Translate program
    rules2 = []
    for rule in program.rules:
        if rule.is_goal():
            r = cratylus.Goal([translate_monomial(table, m, options) for m in rule.clause])
        else:
            r = cratylus.Rule(
                    translate_monomial(table, rule.head, options),
                    [translate_monomial(table, m, options) for m in rule.clause])
        rules2.append(r)

    return '\n'.join(comment) + '\n\n' + cratylus.Program(rules2).repr_compact(OPTIONS['compact'])

def usage():
    sys.stderr.write('Normalize or translate a Cratylus program.\n')
    sys.stderr.write('Copyright (c) 2012 - Pablo Barenbaum <foones@gmail.com>\n')
    sys.stderr.write('Usage:\n')
    sys.stderr.write('    %s <infile.cr> [options]\n' % (sys.argv[0],))
    sys.stderr.write('Options:\n')
    sys.stderr.write('    -o <outfile.cr>   write the results in <outfile>\n')
    sys.stderr.write('    -b                translate a monomial form program to univariate binary polynomial (i.e. in Z_2)\n')
    sys.stderr.write('    -f                translate a monomial form program to FRACTRAN\n')
    sys.stderr.write('    -u                translate a monomial form program to univariate polynomials\n')
    sys.stderr.write('    -v                translate a monomial form program compacting variable names\n')
    sys.stderr.write('    -w                translate a monomial form program using words\n')
    sys.stderr.write('    -p <prefix>       add a prefix to each variable name, useful for linking\n')
    sys.stderr.write('    -t var poly       translate the variable to the given polynomial\n')
    sys.stderr.write('    -nc               do not compact the format of the output\n')
    sys.exit(1)

if __name__ == '__main__':

    args = []
    translation = 'normalize'
    initial_table = {}
    options = {'prefix': None}
    
    outfile = None
    i = 1
    while i < len(sys.argv):
        if sys.argv[i] == '-f':
            translation = 'fractran'
            i += 1
        elif sys.argv[i] == '-u':
            translation = 'univariate'
            i += 1
        elif sys.argv[i] == '-v':
            translation = 'vars_compact'
            i += 1
        elif sys.argv[i] == '-w':
            translation = 'words'
            i += 1
        elif sys.argv[i] == '--mixed':
            translation = 'mixed'
            i += 1
        elif sys.argv[i] == '-b':
            translation = 'binary'
            OPTIONS['target_modulo'] = 2
            i += 1
        elif sys.argv[i] == '-o':
            i += 1
            if i >= len(sys.argv):
                usage()
            outfile = sys.argv[i]
            i += 1
        elif sys.argv[i] == '-t':
            if i + 2 >= len(sys.argv):
                usage()
            i += 1
            var = sys.argv[i]
            poly = sys.argv[i + 1]
            i += 2
            initial_table[var] = poly
        elif sys.argv[i] == '-p':
            i += 1
            if i >= len(sys.argv):
                usage()
            options['prefix'] = sys.argv[i]
            i += 1
        elif sys.argv[i] == '-nc':
            OPTIONS['compact'] = False
            i += 1
        else:
            args.append(sys.argv[i])
            i += 1

    if len(args) != 1:
        usage()

    in_file = args[0]
    if in_file.endswith('.cr2'):
        cratylus.OPTIONS['source_modulo'] = 2

    if in_file.endswith('.crm'):
        cratylus.OPTIONS['allow_maximal_powers'] = True
        initial_table['<'] = '<'
        initial_table['>'] = '>'

    for k, v in initial_table.items():
        initial_table[k] = cratylus.poly_from_string(v, modulo=OPTIONS['target_modulo'])

    f = file(in_file)
    contents = f.read()
    f.close()

    options['initial_table'] = initial_table
    result = translate(in_file, translation, contents, options)

    print result
    if outfile is not None:
        f = file(outfile, 'w')
        f.write(result)
        f.close()

