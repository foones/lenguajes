
OP_ARROW = '_→_'
OP_UNIFY = '_==_'
OP_ALTERNATIVE = '_<>_'
OP_SEQUENCE = '_>>_'

VALUE_UNIT = '()'

TYPE_INT = 'Int'
TYPE_UNIT = '()'

def indent(text, n=2):
    return '\n'.join([' ' * n + line for line in text.split('\n')])

def is_blank(c):
    return c in [' ', '\t', '\r', '\n']

def is_readable(c):
    return c.isprintable() and not is_blank(c)

def is_digit(c):
    return '0' <= c <= '9'

def is_number(str):
    for c in str:
        if not is_digit(c):
            return False
    return True

def is_operator(str):
    return '_' in str

NEXT_INDEX = 0

def fresh_index():
    global NEXT_INDEX
    index = NEXT_INDEX
    NEXT_INDEX += 1
    return index

E_STAGES = {
    'lexer': 'Lexical error near {position}',
    'parser': 'Parse error near {position}',
    'precedence': 'Parse error near {position}',
    'typechecker': 'Type error near {position}',
}

E_MESSAGES = {
    'lexer:no-tabs-allowed':
        'No tabs allowed in the source.',
    'lexer:invalid-character':
        'Invalid character (0x{character}).',
    'lexer:consecutive-underscores':
        'A name cannot contain two consecutive underscores.',
    'lexer:unclosed-multiline-comment':
        'End-of-file was found with unclosed multiline comments.',

    'precedence:not-an-operator':
        'Name "{name}" is not an operator.',
    'precedence:operator-already-exists':
        'Operator "{name}" has already been declared before.',

    'parser:token-mismatch':
        'Expected {expected}, but got {got}.',
    'parser:expected-atom':
        'Expected an atom, but got {got}.',
    'parser:expected-operator-part':
        'Expected part of an operator. Status: {status}.',
    'parser:undeclared-operator':
        'Operator "{name}" has not been declared.',
    'parser:cannot-parse-expression':
        'Cannot parse expression.',
    'parser:operator-part-is-not-a-variable':
        '"{name}" is part of an operator and cannot be used as a variable.',
    'parser:must-be-binary-operator':
        'Operator "{name}" should be binary to be declared associative.',

    'typechecker:data-lhs-arg-variable':
        'Parameter of a datatype should be a variable, but got {got}.',
    'typechecker:data-lhs-type-variable':
        'The name of a datatype should be a variable, but got {got}.',
    'typechecker:data-lhs-type-already-defined':
        'Type "{name}" has already been defined before.',
    'typechecker:expected-a-type':
        'Expected a type, but got "{got}".',
    'typechecker:undefined-type':
        'Type "{name}" is not defined.',
    'typechecker:expected-atomic-kind':
        'Kind of type {type} is "{kind}" but should be "*".',
    'typechecker:constructor-already-defined':
        'Constructor "{name}" has already been defined before.',
    'typechecker:constructor-must-return-instance':
        'Constructor "{constructor_name}" should return an ' +
        'instance of "{type_name}".',
    'typechecker:kinds-do-not-unify':
        'Kinds "{kind1}" and "{kind2}" do not unify.',
    'typechecker:value-already-defined':
        'Value "{name}" has already been defined before.',
    'typechecker:declaration-head-is-not-variable':
        'Head of declaration should be a variable, but found: {head}',
    'typechecker:name-declared-but-not-defined':
        'Name {name} is declared but not defined.',
    'typechecker:equations-arity-mismatch':
        'All equations defining "{name}" should have the same ' +
        'number of parameters.',
    'typechecker:unbound-variable':
        'Unbound variable "{name}".',
    'typechecker:types-do-not-unify':
        'Types "{type1}" and "{type2}" do not unify.',
    'typechecker:occurs-check-fail':
        'Unification\'d give infinite type ("{type1}" vs. "{type2}").',
    'typechecker:malformed-type':
        'Malformed type "{type}".',
}

class LangException(Exception):

    def __init__(self, stage, message, **args):
        self._stage = stage
        self._message = message
        self._args = args

    def __str__(self):
        return '\n'.join([
            '',
            '',
            '--- ERROR ---',
            E_STAGES[self._stage].format(**self._args),
            E_MESSAGES[self._stage + ':' + self._message].format(**self._args)
        ])

class UnificationFailure(Exception):

    def __init__(self, reason, **kwargs):
        self.reason = reason
        self.kwargs = {}
        for attr in kwargs:
            setattr(self, attr, kwargs[attr])
            self.kwargs[attr] = kwargs[attr]

