#!/usr/bin/python

NUM_BASE = 10
STR_BASE = 100

TRAN = {
chr(10): 99,
'G': 98,
'Q': 97,
'e': 96,
'o': 95,
'y': 94,
'=': 93,
'3': 92,
}

def cstring(x):
    power = 0
    r = 0
    for c in x:
        if c in TRAN:
            oc = TRAN[c]
        else:
            oc = ord(c) - 31
        assert 0 <= oc < 100
        assert oc % 10 != 0
        assert oc / 10 != 0
        r = r + oc * (STR_BASE ** power)
        power += 1
    return r

lib = ''

lib += '''DivmodDm_c^@=>Divmod.DivmodX^@=>Dm__hDm_c^@X^@.Divmod=>Dm__h.Dm__hC^@=>Dm_z.
Dm__h=>Dm_z.Dm_zR^@=>Dm_s.Dm_z=>Dm_s.Dm_cDm_s=>Dm_cDm_l.Dm_s=>Dm_r.
Dm_d^@Dm_l=>Dm_l.Dm_lY^@=>Dm__fDm_d^@Y^@.Dm_l=>Dm__f.Dm__fDm_e^@=>Dm_eDm_f.
Dm__f=>Dm_eDm_f.Dm_b^@Dm_f=>Dm_f.Dm_d^@Dm_f=>Dm__sDm_b^@Dm_d^@.Dm_f=>Dm__s.
Dm__s=>Dm_aDm_j.Dm_aDm_b^@Dm_j=>Dm__gDm_g^@Dm_h^@.Dm_a^@Dm_j=>Dm_i.Dm_j=>Dm_i.
Dm__gDm_h^@=>Dm__gDm_b^@.Dm__gDm_g^@=>Dm_b^@Dm_j.Dm_iDm_k^@=>Dm_i.
Dm_c^@Dm_i=>Dm__dDm_c^@Dm_k^@.Dm_i=>Dm__d.Dm__dDm_b^@=>Dm__pDm_a^@Dm_b^@.
Dm__d=>Dm_q.Dm__pDm_a^@Dm_k^@=>Dm__i.Dm__p=>Dm__i.Dm__iDm_a^@=>Dm_q.Dm__i=>Dm_q.
Dm_kDm_q=>Dm__uDm_k.Dm_q=>Dm_o.Dm__u=>Dm_aDm_t.Dm_aDm_e^@Dm_t=>Dm_g^@Dm_h^@Dm_w.
Dm_a^@Dm_t=>Dm__l.Dm_t=>Dm__l.Dm_h^@Dm_w=>Dm_e^@Dm_w.Dm_g^@Dm_w=>Dm_e^@Dm_t.
Dm__l=>Dm_aDm_u.Dm_aDm_d^@Dm_u=>Dm_g^@Dm_h^@Dm_x.Dm_a^@Dm_u=>Dm__n.Dm_u=>Dm__n.
Dm_h^@Dm_x=>Dm_d^@Dm_x.Dm_g^@Dm_x=>Dm_d^@Dm_u.Dm__n=>Dm_f.Dm_b^@Dm_o=>Dm_o.
Dm_d^@Dm_o=>Dm__kDm_b^@Dm_d^@.Dm_o=>Dm__k.Dm__kDm_c^@=>Dm__rDm_a^@Dm_c^@.
Dm__k=>Dm_p.Dm__rDm_a^@Dm_b^@=>Dm__c.Dm__r=>Dm__c.Dm__cDm_a^@=>Dm_p.Dm__c=>Dm_p.
Dm_bDm_p=>Dm_bDm_m.Dm_p=>Dm__o.Dm__oDm_d^@=>Dm__tDm_a^@Dm_d^@.Dm__o=>Dm_n.
Dm__tDm_a^@Dm_c^@=>Dm__e.Dm__t=>Dm__e.Dm__eDm_a^@=>Dm_n.Dm__e=>Dm_n.
Dm_e^@Dm_n=>Dm__mDm_e^@C^@.Dm_n=>Dm__m.Dm__m=>Dm_s.Dm_mR^@=>Dm_m.
Dm_c^@Dm_m=>Dm_c^@Dm_rR^@.Dm_m=>Dm_r.Dm_d^@Dm_r=>Dm__a.Dm_r=>Dm__a.
Dm__aDm_b^@=>Dm_v.Dm__a=>Dm_v.Dm_e^@Dm_v=>Dm__b.Dm_v=>Dm__b.Dm__bDm_c^@=>Dm__j.
Dm__b=>Dm__j.Dm__jX^@=>Dm_y.Dm__j=>Dm_y.Dm_yY^@=>Dm__q.Dm_y=>Dm__q.Dm__q.\n'''

enable = True

if enable:
    rad = 'In=>Ina.'  #### XXX: enable for real quine
else:
    rad = 'InX^@.' #### XXX: disable for real quine

## Print number
lastname = ''
for name, power in [('a', 512), ('b', 256), ('c', 128), ('d', 64), ('e', 32)]:
    rad += 'In%sX^@=>Mn%sDivmodX^@Y^%u.' % (name, name, NUM_BASE ** power)
    if lastname == '':
        rad += 'In%s.' % (name,)
    else:
        rad += 'In%s=>Ln%s.' % (name, lastname)
    rad += 'Mn%sC^@=>Mn%sX%s^@.' % (name, name, name,)
    rad += 'Mn%sR^@=>In%sX^@.' % (name, chr(ord(name) + 1),)
    rad += 'Ln%sX%s^@=>In%sX^@.' % (name, name, name)
    rad += 'Ln%s=>In%s.' % (name, name)
    rad += '\n'
    lastname = name
name = chr(ord(lastname) + 1)
rad += 'In%sX^@=>DivmodX^@Y^%uJn.' % (name, NUM_BASE)
rad += 'In%s=>Ln%s.' % (name, lastname)
rad += 'JnR^@=>KnS^@T^48.'
rad += 'Jn=>KnT^48.'
rad += 'KnS^@=>KnT^@.'
rad += 'KnT^@=>Kn>^@.'
rad += 'KnC^@=>KnX^@.'
rad += 'Kn=>In%s.' % (name,)
rad += '\n'

lib += rad

## Print string
if enable:
    rad = 'Is=>Isa.'  #### XXX: enable for real quine
else:
    rad = 'IsX^@.' #### XXX: disable for real quine

lastname = ''
for name, power in [('a', 256), ('b', 128), ('c', 64), ('d', 32)]:
    rad += 'Is%sX^@=>Ms%sDivmodX^@Y^%u.' % (name, name, STR_BASE ** power)
    if lastname == '':
        rad += 'Is%s.' % (name,)
    else:
        rad += 'Is%s=>Ls%s.' % (name, lastname)
    rad += 'Ms%sC^@=>Ms%sX%s^@.' % (name, name, name,)
    rad += 'Ms%sR^@=>Is%sX^@.' % (name, chr(ord(name) + 1),)
    rad += 'Ls%sX%s^@=>Is%sX^@.' % (name, name, name)
    rad += 'Ls%s=>Is%s.' % (name, name)
    rad += '\n'
    lastname = name
name = chr(ord(lastname) + 1)
rad += 'Is%sX^@=>DivmodX^@Y^%uJs.' % (name, STR_BASE)
rad += 'Is%s=>Ls%s.' % (name, lastname)
##rad += 'JsR^@=>Js>^@.'
for k, v in sorted(TRAN.items(), key=lambda (k, v): -v):
    rad += '\nJsR^%u=>Ks>^%u.' % (v, ord(k))
rad += '\nJs=>KsR^31.'
rad += 'KsR^@=>Ks>^@.'
rad += 'KsC^@=>Is%sX^@.' % (name,)
rad += 'Ks=>Is%s.' % (name,)

lib += rad

nchunks = 10

lst = 0
for i in range(nchunks):
    # Quine main
    a = lst
    b = lst + 1
    c = lst + 2
    d = lst + 3
    e = lst + 4
    f = lst + 5
    lib += '\n' + ''.join([
        'Q%u=>Q%uIsX^%s.' % (a, b, cstring('Da%u=>X^' % (i,))),
        'Q%u=>Q%uInDz%u.' % (b, c, i),
        'Q%u=>Q%uIsX^%s.' % (c, d, cstring('.\nDz%u=>X^' % (i,))),
        'Q%u=>Q%uInDa%u.' % (d, e, i),
        'Q%u=>Q%uIsX^%s.' % (e, f, cstring('.\n')),
    ])
    lst = f

for i in range(nchunks):
    a = lst
    b = lst + 1
    lib += '\n' + ''.join([
        'Q%u=>Q%uIsDa%u.' % (a, b, i),
    ])
    lst = b

def badchunk(chunk):
    s = str(cstring(chunk))
    return s[0] == '0' or s[-1] == '0'

lib += '?Q0.'

chunksize = (len(lib) + nchunks - 1) / nchunks

head = ''
libcopy = lib
for i in range(nchunks):
    cs = chunksize
    while badchunk(libcopy[:cs]):
        cs += 1
    chunk = libcopy[:cs]
    libcopy = libcopy[cs:]
    s = cstring(chunk)
    head += 'Da%u=>X^%s.\n' % (i, s)
    head += 'Dz%u=>X^%s.\n' % (i, ''.join(reversed(str(s))))
assert libcopy == ''

print head + lib
#print '#CHUNKSIZE', chunksize
