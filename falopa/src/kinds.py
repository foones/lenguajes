
import common

class Type:

    def __init__(self, attributes, **kwargs):
        assert sorted(kwargs.keys()) == sorted(attributes)
        self._attributes = attributes
        for attr in attributes:
            setattr(self, attr, kwargs[attr])

    def representative(self):
        return self

    def is_set(self):
        return False

    def is_fun(self):
        return False

    def is_metavar(self):
        return False

class Set(Type):

    def __init__(self, **kwargs):
        Type.__init__(self, [], **kwargs)

    def __repr__(self):
        return '*'

    def is_set(self):
        return True

    def free_metavars(self):
        return set()

class Fun(Type):

    def __init__(self, **kwargs):
        Type.__init__(self, ['domain', 'codomain'], **kwargs)

    def __repr__(self):
        return '({domain} -> {codomain})'.format(
                   domain=self.domain,
                   codomain=self.codomain
               )

    def is_fun(self):
        return True

    def free_metavars(self):
        return self.domain.free_metavars() | self.codomain.free_metavars()

class Metavar(Type):

    def __init__(self, prefix='x'):
        Type.__init__(self, ['prefix', 'index'],
                            prefix=prefix, index=common.fresh_index())
        self._indirection = None

    def __repr__(self):
        if self._indirection is None:
            return '?{prefix}{index}'.format(
                       prefix=self.prefix,
                       index=self.index
                   )
        else:
            return repr(self._indirection)

    def is_metavar(self):
        return True

    def representative(self):
        if self._indirection is None:
            return self
        else:
            self._indirection = self._indirection.representative()
            return self._indirection

    def instantiate(self, value):
        assert self._indirection is None
        self._indirection = value

    def free_metavars(self):
        if self._indirection is None:
            return set([self])
        else:
            return self._indirection.free_metavars()

# Return a kind of the form:
#   ?k1 -> ... -> ?kn -> *
def fresh_kind(arity):
    kind = Set()
    for i in range(arity):
        kind = Fun(domain=Metavar(prefix='k'), codomain=kind)
    return kind

def unify(k1, k2):
    k1 = k1.representative()
    k2 = k2.representative()
    if k1.is_metavar():
        if k1 == k2:
            return
        if k1 in k2.free_metavars():
            raise common.UnificationFailure('kind-occurs-check',
                                            kind1=k1, kind2=k2)
        k1.instantiate(k2)
    elif k2.is_metavar():
        unify(k2, k1)
    elif k1.is_set() and k2.is_set():
        return
    elif k1.is_fun() and k2.is_fun():
        unify(k1.domain, k2.domain)
        unify(k1.codomain, k2.codomain)
    else:
        raise common.UnificationFailure('kinds-do-not-unify',
                                        kind1=k1, kind2=k2)

