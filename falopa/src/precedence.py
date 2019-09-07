import bisect

import common
import lexer

DEFAULT_PRECEDENCE = 200

class PrecedenceLevel:

    def __init__(self, fixity, precedence):
        self._fixity = fixity
        self._precedence = precedence
        self._operators = set([])

    def key(self):
        return self._precedence, self._fixity

    def fixity(self):
        return self._fixity

    def declare_operator(self, name):
        self._operators.add(name)

    def operators(self):
        return self._operators

    def __repr__(self):
        return str(self._operators)

def is_prefix(list1, list2):
    return len(list1) <= len(list2) and list2[:len(list1)] == list1

class PrecedenceTable:

    def __init__(self):
        self._table = {}
        self._table_keys = []
        self._operators = set([])
        self._parts = set([])

    def declare_operator(self, fixity, precedence, name, position=None):
        if not common.is_operator(name):
            self.fail('not-an-operator', name=name, position=position)
        if name in self._operators:
            self.fail('operator-already-exists', name=name, position=position)

        for part in lexer.operator_to_parts(name):
            if part != '':
                self._parts.add(part)

        self._operators.add(name)
        key = precedence, fixity

        if key not in self._table:
            self._table[key] = PrecedenceLevel(fixity, precedence)
            self._table_keys = sorted(self._table.keys())
        self._table[key].declare_operator(name)

    def fixity(self, key):
        return self._table[key].fixity()

    def is_declared_operator(self, name):
        return name in self._operators

    def is_declared_part(self, name):
        return name in self._parts

    def is_status_in_level(self, key, status):
        level = self._table[key]
        for operator in level.operators():
            if status == lexer.operator_to_parts(operator):
                return True
        return False

    def is_status_prefix_in_level(self, key, status):
        if status in [[], ['']]:
            return True
        level = self._table[key]
        for operator in level.operators():
            if is_prefix(status, lexer.operator_to_parts(operator)):
                return True
        return False

    def is_binop_in_level(self, key, name):
        level = self._table[key]
        for operator in level.operators():
            if lexer.operator_to_parts(operator) == ['', name, '']:
                return True
        return False

    def first_level(self):
        if len(self._table_keys) == 0:
            return None
        else:
            return self._table_keys[0]

    def next_level(self, key):
        index = bisect.bisect_left(self._table_keys, key)
        if index < len(self._table_keys) - 1:
            return self._table_keys[index + 1]
        else:
            return None

    def fail(self, msg, **args):
        raise common.LangException(
                'precedence',
                msg,
                **args
              )

