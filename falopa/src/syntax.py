import common
import lexer

class AST:
    "Base class for all syntactic constructs."

    def __init__(self, attributes, **kwargs):
        if 'position' in kwargs:
            self.position = kwargs['position']
            del kwargs['position']
        else:
            self.position = None

        assert sorted(kwargs.keys()) == sorted(attributes)
        self._attributes = attributes
        for attr in attributes:
            setattr(self, attr, kwargs[attr])

    def __repr__(self):
        attrs = []
        for attr in self._attributes:
            attrs.append(
                '{attr}={value}'.format(
                    attr=attr,
                    value=repr(getattr(self, attr))
                )
            )
        return '{cls}({attrs})'.format(
                    cls=self.__class__.__name__,
                    attrs=', '.join(attrs)
               )

    def is_data_declaration(self):
        return False

    def is_type_declaration(self):
        return False

    def is_definition(self):
        return False

    def is_application(self):
        return False

    def is_variable(self):
        return False

    def is_integer_constant(self):
        return False

    def is_fresh(self):
        return False

    def is_lambda(self):
        return False

    def is_let(self):
        return False

    def is_forall(self):
        return False

    def is_metavar(self):
        return False

    def is_atom(self):
        return False

    def is_arrow_type(self):
        return self.is_application() and \
               self.fun.is_application() and \
               self.fun.fun.is_variable() and \
               self.fun.fun.name == common.OP_ARROW

    def free_variables(self):
        return set()

    def free_metavars(self):
        return set()

    def forall_introduce(self, metavar):
        var = fresh_variable(prefix=metavar.prefix, position=self.position)
        return Forall(var=var.name,
                      body=self.instantiate_metavar(metavar, var),
                      position=self.position)

    def representative(self):
        return self

    def application_head(self):
        expr = self.representative()
        while expr.is_application():
            expr = expr.fun.representative()
        return expr

    def application_args(self):
        expr = self.representative()
        args = []
        while expr.is_application():
            args.insert(0, expr.arg.representative())
            expr = expr.fun.representative()
        return args

    def instantiate_type_variable(self, name, value):
        return self

    def instantiate_metavar(self, metavar, value):
        return self

    def pprint(self, level=0):
        indent = ' ' * level
        attrs = []
        for attr in self._attributes:
            next_level = level + 2
            attrs.append(
                '{indent}  {attr}={value}\n'.format(
                    attr=attr,
                    value=pprint(getattr(self, attr), level=next_level),
                    indent=indent,
                )
            )
        if len(attrs) == 0:
            args = ''
        else:
            args = '(\n{attrs}{indent})'.format(
              attrs=''.join(attrs),
              indent=indent,
            )

        return self.__class__.__name__ + args

    def show(self):
        return self.pprint()

    def showp(self):
        if self.is_atom():
            return self.show()
        else:
            return '(' + self.show() + ')'

def pprint(x, level=0):
    if isinstance(x, AST):
        return x.pprint(level=level)
    elif isinstance(x, list):
        if len(x) == 0:
            return '[]'
        else:
            return '[\n' + \
                   '\n'.join([
                     (level + 2) * ' ' + pprint(y, level=level + 2)
                     for y in x
                    ]) + \
                    '\n' + level * ' ' + ']'
    else:
        return repr(x)

# Program

class Program(AST):

    def __init__(self, **kwargs):
        AST.__init__(self,
                     ['data_declarations', 'body'],
                     **kwargs)

    def show(self):
        lines = []
        for decl in self.data_declarations:
            lines.append(decl.show())
            lines.append('')
        lines.append(self.body.show())
        return '\n'.join(lines)

# Declarations

class DataDeclaration(AST):

    def __init__(self, **kwargs):
        AST.__init__(self, ['lhs', 'constructors'], **kwargs)

    def is_data_declaration(self):
        return True

    def show(self):
        lines = []
        lines.append('data {lhs} where'.format(lhs=self.lhs.show()))
        for decl in self.constructors:
            lines.append('  {decl}'.format(decl=decl.show()))
        return '\n'.join(lines)

class TypeDeclaration(AST):

    def __init__(self, **kwargs):
        AST.__init__(self, ['name', 'type'], **kwargs)

    def is_type_declaration(self):
        return True

    def show(self):
        return '{name} : {type}'.format(
                 name=self.name,
                 type=self.type.show()
               )

class Definition(AST):

    def __init__(self, **kwargs):
        AST.__init__(self, ['lhs', 'rhs', 'where'], **kwargs)

    def is_definition(self):
        return True

    def show(self):
        lines = [self.lhs.show() + ' = ' + self.rhs.show()]
        if len(self.where) > 0:
          lines.append('  where')
          for decl in self.where:
            lines.append(common.indent(decl.show(), 4))
        return '\n'.join(lines)

    def free_variables(self):
        fvs = set()
        fvs |= self.lhs.free_variables()
        fvs |= Let(declarations=self.where, body=self.rhs).free_variables()
        return fvs

# Expressions

class IntegerConstant(AST):

    def __init__(self, **kwargs):
        AST.__init__(self, ['value'], **kwargs)

    def free_variables(self):
        return set()

    def free_metavars(self):
        return set()

    def show(self):
        return str(self.value)

    def is_atom(self):
        return True

    def is_integer_constant(self):
        return True

class Variable(AST):

    def __init__(self, **kwargs):
        AST.__init__(self, ['name'], **kwargs)

    def is_variable(self):
        return True

    def free_variables(self):
        return set([self.name])

    def free_metavars(self):
        return set()

    def show(self):
        return self.name

    def is_atom(self):
        return True

    def instantiate_type_variable(self, name, value):
        if self.name == name:
            return value
        else:
            return self

    def instantiate_metavar(self, metavar, value):
        return self

def primitive_type_int():
    return Variable(name=common.TYPE_INT, position=None)

def primitive_type_unit():
    return Variable(name=common.TYPE_UNIT, position=None)

def fresh_variable(prefix='x', position=None):
    return Variable(name='{prefix}{index}.'.format(
                            prefix=prefix,
                            index=common.fresh_index()),
                    position=position)

class Application(AST):

    def __init__(self, **kwargs):
        AST.__init__(self, ['fun', 'arg'], **kwargs)

    def is_application(self):
        return True

    def free_variables(self):
        return self.fun.free_variables() | self.arg.free_variables()

    def free_metavars(self):
        return self.fun.free_metavars() | self.arg.free_metavars()

    def show(self):
        if self.is_arrow_type():
            return self.show_arrow_type()
        head = self.application_head()
        args = self.application_args()
        wrap_head = True
        if head.is_variable():
            arity = self._operator_arity(head.name)
            if arity == 0:
                wrap_head = False
            if len(args) >= arity:
                head = self._show_mixfix(head.name, args[:arity])
                args = args[arity:]
        else:
            head = head.show()
        if len(args) == 0:
            return head
        else:
            if wrap_head:
                head = '({head})'.format(head=head)
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

    def show_arrow_type(self):
        parts = []
        res = self
        while res.is_arrow_type():
            parts.append(res.fun.arg.showp())
            res = res.arg
        parts.append(res.show())
        [_, arrow, _] = lexer.operator_to_parts(common.OP_ARROW)
        return ' {arrow} '.format(arrow=arrow).join(parts)

    def instantiate_type_variable(self, name, value):
        return Application(
                 fun=self.fun.instantiate_type_variable(name, value),
                 arg=self.arg.instantiate_type_variable(name, value),
                 position=self.position
               )

    def instantiate_metavar(self, metavar, value):
        return Application(
                 fun=self.fun.instantiate_metavar(metavar, value),
                 arg=self.arg.instantiate_metavar(metavar, value),
                 position=self.position
               )

def application_many(fun, args, position=None):
    if position is None:
        position = fun.position
    for arg in args:
        fun = Application(fun=fun, arg=arg, position=position) 
    return fun

def function(arg_type, result_type, position=None):
    if position is None:
        position = arg_type.position
    return Application(
             fun=Application(
               fun=Variable(name=common.OP_ARROW, position=position),
               arg=arg_type,
               position=position
             ),
             arg=result_type,
             position=position
           )

def function_many(arg_types, result_type, position=None):
    for arg_type in reversed(arg_types):
        result_type = function(arg_type, result_type, position=position)
    return result_type

def binop(op, e1, e2, position=None):
    if position is None:
        position = e1.position
    return Application(
             fun=Application(
               fun=Variable(name=op, position=position),
               arg=e1,
               position=position
             ),
             arg=e2,
             position=position
           )

def unify(e1, e2, position=None):
    return binop(common.OP_UNIFY, e1, e2, position=position)

def sequence(e1, e2, position=None):
    return binop(common.OP_SEQUENCE, e1, e2, position=position)

def alternative(e1, e2, position=None):
    return binop(common.OP_ALTERNATIVE, e1, e2, position=position)

def sequence_many1(es, body, position=None):
    if position is None:
        position = body.position
    for e in reversed(es):
        body = sequence(e, body, position=position)
    return body

def alternative_many(es, position=None):
    assert len(es) > 0
    if position is None:
        position = es[0].position
    body = es[-1]
    for e in reversed(es[:-1]):
        body = alternative(e, body, position=position)
    return body

class Lambda(AST):

    def __init__(self, **kwargs):
        AST.__init__(self, ['var', 'body'], **kwargs)

    def free_variables(self):
        return self.body.free_variables() - set([self.var])

    def is_lambda(self):
        return True

    def show(self):
        return 'λ {var} . {body}'.format(
                 var=self.var,
                 body=self.body.show()
               )

def lambda_many(vars, body, position=None):
    if position is None:
        position = body.position
    for var in reversed(vars):
        body = Lambda(var=var, body=body, position=position)
    return body

class Fresh(AST):

    def __init__(self, **kwargs):
        AST.__init__(self, ['var', 'body'], **kwargs)

    def free_variables(self):
        return self.body.free_variables() - set([self.var])

    def is_fresh(self):
        return True

    def show(self):
        return '? {var} . {body}'.format(
                 var=self.var,
                 body=self.body.show()
               )

def fresh_many(vars, body, position=None):
    if position is None:
        position = body.position
    for var in vars:
        body = Fresh(var=var, body=body, position=position)
    return body

class Let(AST):

    def __init__(self, **kwargs):
        AST.__init__(self, ['declarations', 'body'], **kwargs)

    def is_let(self):
        return True

    def free_variables(self):
        fvs = set()

        bvs = set()
        for decl in self.declarations:
            if not decl.is_definition():
                continue
            head = decl.lhs.application_head()
            if head.is_variable():
                bvs.add(head.name)

        for decl in self.declarations:
            if not decl.is_definition():
                continue
            fvs |= decl.free_variables()
        fvs |= self.body.free_variables()
        fvs = fvs - bvs
        return fvs

    def show(self):
        lines = []
        lines.append('let')
        i = 0
        for decl in self.declarations:
            lines.append(common.indent(decl.show(), 4))
            i += 1
            if i < len(self.declarations) and not decl.is_type_declaration():
                lines.append('')
        lines.append(' in')
        lines.append(common.indent(self.body.show(), 4))
        return '\n'.join(lines)

# Only at the type level
class Forall(AST):

    def __init__(self, **kwargs):
        AST.__init__(self, ['var', 'body'], **kwargs)

    def is_forall(self):
        return True

    def forall_eliminate(self, value=None):
        if value is None:
            value = Metavar(prefix=self.var, position=self.position)
        return self.body.instantiate_type_variable(self.var, value)

    def free_variables(self):
        return self.body.free_variables() - set([self.var])

    def free_metavars(self):
        return self.body.free_metavars()

    def show(self):
        return '∀ {var} . {body}'.format(
                 var=self.var,
                 body=self.body.show()
               )

    def instantiate_type_variable(self, name, value):
        if self.var == name:
            return self
        else:
            return Forall(
                     var=self.var,
                     body=self.body.instantiate_type_variable(name, value),
                     position=self.position
                   )

    def instantiate_metavar(self, metavar, value):
        return Forall(
                 var=self.var,
                 body=self.body.instantiate_metavar(metavar, value),
                 position=self.position
               )

def forall_many(vars, expr, position=None):
    if position is None:
        position = expr.position
    for var in vars:
        expr = Forall(var=var, body=expr, position=position)
    return expr

class Metavar(AST):

    def __init__(self, prefix='x', **kwargs):
        AST.__init__(self, ['prefix', 'index'],
                           prefix=prefix, index=common.fresh_index(),
                           **kwargs)
        self._indirection = None

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

    def instantiate_type_variable(self, name, value):
        if self._indirection is None:
            return self
        else:
            return self._indirection.instantiate_type_variable(name, value)

    def instantiate_metavar(self, metavar, value):
        if self == metavar:
            return value
        elif self._indirection is None:
            return self
        else:
            return self._indirection.instantiate_metavar(metavar, value)

    def free_variables(self):
        if self._indirection is None:
            return set()
        else:
            return self._indirection.free_variables()

    def free_metavars(self):
        if self._indirection is None:
            return set([self])
        else:
            return self._indirection.free_metavars()

    def show(self):
        if self._indirection is None:
            return '?{prefix}{index}'.format(
                     prefix=self.prefix,
                     index=self.index,
                   )
        else:
            return self._indirection.show()

def free_variables_list(es):
    fvs = set()
    for e in es:
        fvs |= e.free_variables()
    return fvs

##

def unify_types(t1, t2):
    # Valid types are built using:
    #   Variable
    #   Application
    #   Forall
    #   Metavar
    # Any other type expression is rejected.
    t1 = t1.representative()
    t2 = t2.representative()
    if t1.is_metavar():
        if t1 == t2:
            return
        if t1 in t2.free_metavars():
            raise common.UnificationFailure('occurs-check-fail',
                                            type1=t1.show(),
                                            type2=t2.show())
        return t1.instantiate(t2)
    elif t2.is_metavar():
        return unify_types(t2, t1)

    head1 = t1.application_head()
    args1 = t1.application_args()
    head2 = t2.application_head()
    args2 = t2.application_args()
    if not head1.is_variable():
        raise common.UnificationFailure('malformed-type', type=t1.show())
    if not head2.is_variable():
        raise common.UnificationFailure('malformed-type', type=t2.show())
    if head1.name != head2.name or len(args1) != len(args2):
        raise common.UnificationFailure(
                'types-do-not-unify',
                type1=t1.show(),
                type2=t2.show())
    for s1, s2 in zip(args1, args2):
        unify_types(s1, s2)

