import lexer
import common

####

class Value:

    def __init__(self):
        self.position = None

    def is_thunk(self):
        return False

    def is_integer_constant(self):
        return False

    def is_rigid_structure(self):
        return False

    def is_flex_structure(self):
        return False

    def is_primitive(self):
        return False

    def is_closure(self):
        return False

    def is_rigid(self):
        return False

    def is_decided(self):
        return True

    def is_atom(self):
        return False

    def representative(self):
        return self

    def showp(self):
        s = self.show()
        if not self.is_atom():
            s = '(' + s + ')'
        return s

    def show_application(self, head, args):
        arity = self._operator_arity(head)
        if arity == 0:
            wrap_head = False
        if len(args) >= arity:
            head = self._show_mixfix(head, args[:arity])
            args = args[arity:]
        if len(args) == 0:
            return head
        else:
            if wrap_head:
                head = '(' + head + ')'
            return head + ' ' + ' '.join([arg.showp() for arg in args])

    def _operator_arity(self, opr):
        arity = 0
        for part in lexer.operator_to_parts(opr):
            if part == '':
                arity += 1
        return arity

    def _show_mixfix(self, opr, args):
        res = []
        for part in lexer.operator_to_parts(opr):
            if part == '':
                res.append(args.pop(0).showp())
            else:
                res.append(part)
        return ' '.join(res)

    def free_metavars(self):
        return set()

class Metavar(Value):

    def __init__(self, prefix='x', **kwargs):
        Value.__init__(self)
        self.prefix = prefix
        self.index = common.fresh_index()
        self._indirection = None

    def representative(self):
        if self._indirection is None:
            return self
        else:
            self._indirection = self._indirection.representative()
            return self._indirection

    def instantiate(self, value):
        assert self._indirection is None
        self._indirection = value

    def uninstantiate(self):
        self._indirection = None

    def show(self):
        if self._indirection is None:
            return '?{prefix}{index}'.format(
                       prefix=self.prefix,
                       index=self.index
                   )
        else:
            return self._indirection.show()

    def is_instantiated(self):
        return self._indirection is not None

    def free_metavars(self):
        if self._indirection is None:
            return set([self])
        else:
            return self._indirection.free_metavars()

    def is_strongly_decided(self):
        if self._indirection is None:
            return True
        else:
            return self._indirection.is_strongly_decided()

class Thunk(Value):
    "Represents a suspended computation."

    def __init__(self, expr, env):
        Value.__init__(self)
        self.expr = expr
        self.env = env

    def show(self):
        return '({expr})@...'.format(
                 expr=self.expr.show(),
                 #env=self.env.show()
               )

    def is_thunk(self):
        return True

    def is_decided(self):
        return False

    def is_atom(self):
        return True

    def is_strongly_decided(self):
        return False

class IntegerConstant(Value):
    "Represents a number."

    def __init__(self, value):
        Value.__init__(self)
        self.value = value

    def show(self):
        return '{n}'.format(n=self.value)

    def is_integer_constant(self):
        return True

    def is_rigid(self):
        return True

    def is_atom(self):
        return True

    def is_strongly_decided(self):
        return True

class RigidStructure(Value):
    "Represents a constructor applied to a number of arguments."

    def __init__(self, constructor, args):
        Value.__init__(self)
        self.constructor = constructor
        self.args = args

    def show(self):
        return self.show_application(self.constructor, self.args)

    def is_rigid_structure(self):
        return True

    def is_rigid(self):
        return True

    def is_atom(self):
        return len(self.args) == 0

    def free_metavars(self):
        fmvs = set()
        for arg in self.args:
            fmvs |= arg.free_metavars()
        return fmvs

    def is_strongly_decided(self):
        return all([arg.is_strongly_decided() for arg in self.args])

def unit():
    return RigidStructure(common.VALUE_UNIT, [])

class UniversalVariable:

    def __init__(self, prefix='x'):
        self.prefix = prefix
        self.index = common.fresh_index()

    def show(self):
        return '${prefix}.{index}'.format(
                 prefix=self.prefix,
                 index=self.index
               )

class UniversalStructure(Value):
    """Represents a universally quantified vaiable applied to
       a number of arguments."""

    def __init__(self, uvar, args):
        Value.__init__(self)
        self.uvar = uvar
        self.args = args

    def show(self):
        return self.show_application(self.uvar.show(), self.args)

    def is_rigid(self):
        return True

    def is_atom(self):
        return len(self.args) == 0

    def free_metavars(self):
        fmvs = set()
        for arg in self.args:
            fmvs |= arg.free_metavars()
        return fmvs

    def is_strongly_decided(self):
        return all([arg.is_strongly_decided() for arg in self.args])

class FlexStructure(Value):
    "Represents a symbolic variable applied to a number of arguments."

    def __init__(self, symbol, args):
        Value.__init__(self)
        self.symbol = symbol
        self.args = args

    def show(self):
        if not self.symbol.is_instantiated():
            return self.show_application(self.symbol.show(), self.args)
        else:
            return ' '.join(
                     ['{fun}'.format(fun=self.symbol.show())] +
                     self.args
                   )

    def is_flex_structure(self):
        return True

    def is_decided(self):
        # The value is decided if the symbol is flex, that is,
        # it has NOT been instantiated yet.
        return not self.symbol.is_instantiated()

    def is_atom(self):
        return len(self.args) == 0 and self.is_decided()

    def free_metavars(self):
        fmvs = self.symbol.free_metavars()
        for arg in self.args:
            fmvs |= arg.free_metavars()
        return fmvs

    def is_strongly_decided(self):
        return self.is_decided() and \
               all([arg.is_strongly_decided() for arg in self.args])

class Primitive(Value):
    """"Represents a partially applied (but *not* fully applied) primitive.
        such as (_>>_ foo) or (_+_ 10)."""

    def __init__(self, name, args):
        Value.__init__(self)
        self.name = name
        self.args = args

    def is_primitive(self):
        return True

    def is_rigid(self):
        return True

    def show(self):
        return self.show_application(self.name, self.args)

    def is_atom(self):
        return len(self.args) == 0

    def free_metavars(self):
        fmvs = set()
        for arg in self.args:
            fmvs |= arg.free_metavars()
        return fmvs

    def is_strongly_decided(self):
        return True

class Closure(Value):
    "Represents a closure (lambda function enclosed in an environment)."

    def __init__(self, var, body, env):
        Value.__init__(self)
        self.var = var
        self.body = body
        self.env = env

    def show(self):
        return '(λ {var} . {body})@...'.format(
                 var=self.var,
                 body=self.body.show()
               )

    def is_closure(self):
        return True

    def is_rigid(self):
        return True

    def is_atom(self):
        return True

    def is_strongly_decided(self):
        return True

