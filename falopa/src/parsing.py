import common
import token
import lexer
import precedence
import syntax

def is_binary_operator(name):
    parts = lexer.operator_to_parts(name)
    return len(parts) == 3 \
       and parts[0] == '' \
       and parts[1] != '' \
       and parts[2] == ''

class Parser:

    def __init__(self, source, filename='...'):
        self._token_stream = lexer.Lexer(source, filename=filename).tokens()
        self._prectable = precedence.PrecedenceTable()
        self.next_token()

        # Primitive operators
        self.declare_operator(token.INFIXR, 50, common.OP_ARROW)
        self.declare_operator(token.INFIXR, 100, common.OP_ALTERNATIVE)
        self.declare_operator(token.INFIXR, 150, common.OP_SEQUENCE)
        self.declare_operator(token.INFIXR, 200, common.OP_UNIFY)

    def parse_program(self):
        position = self.current_position()
        self.match(token.BEGIN)
        data_declarations = []
        value_declarations = []
        while self._token.type() == token.DELIM:
            self.match(token.DELIM)
            for decl in self.parse_toplevel_declaration():
                if decl.is_data_declaration():
                    data_declarations.append(decl)
                else:
                    value_declarations.append(decl)
        self.match(token.END)
        self.match(token.EOF)
        return syntax.Program(
                 data_declarations=data_declarations,
                 body=syntax.Let(declarations=value_declarations,
                                 body=syntax.Variable(name="main",
                                                      position=position),
                                 position=position),
                 position=position,
               )

    def parse_toplevel_declaration(self):
        if self._token.type() in [token.INFIX, token.INFIXR, token.INFIXL]:
            self.parse_fixity_declaration()
            return []
        elif self._token.type() == token.DATA:
            return [self.parse_data_declaration()]
        else:
            return [self.parse_value_declaration()]

    def parse_fixity_declaration(self):
        position = self.current_position()
        fixity = self._token.type()
        self.match_any([token.INFIX, token.INFIXR, token.INFIXL])
        precedence = self.parse_num()
        name = self._token.value() # Do not use self.parse_id() here.
        self.match(token.ID)
        if fixity in [token.INFIXL, token.INFIXR] and \
           not is_binary_operator(name):
            self.fail('must-be-binary-operator', name=name)
        self.declare_operator(fixity, precedence, name, position=position)

    def parse_data_declaration(self):
        position = self.current_position()
        self.match(token.DATA)
        lhs = self.parse_expression()
        self.match(token.WHERE)
        constructors = self.parse_constructor_declarations()
        return syntax.DataDeclaration(lhs=lhs,
                                      constructors=constructors,
                                      position=position)

    def parse_value_declarations(self):
        declarations = []
        self.match(token.BEGIN)
        while self._token.type() == token.DELIM:
            self.match(token.DELIM)
            declarations.append(self.parse_value_declaration())
        self.match(token.END)
        return declarations

    def parse_value_declaration(self):
        if self._token.type() == token.ID:
            tok = self._token
            self.next_token()
            if self._token.type() == token.COLON:
                self.unshift(tok)
                return self.parse_type_declaration()
            else:
                self.unshift(tok)
        return self.parse_declaration()

    def parse_type_declaration(self):
        position = self.current_position()
        name = self._token.value() # Do not use self.parse_id() here.
        self.match(token.ID)
        if common.is_operator(name) and not self.is_declared_operator(name):
            fixity = token.INFIXL if is_binary_operator(name) else token.INFIX
            self.declare_operator(fixity,
                                  precedence.DEFAULT_PRECEDENCE,
                                  name)

        self.match(token.COLON)
        type = self.parse_expression()
        return syntax.TypeDeclaration(name=name, type=type, position=position)

    def parse_declaration(self):
        position = self.current_position()
        lhs = self.parse_expression()
        self.match(token.EQ)
        rhs = self.parse_expression()
        if self._token.type() == token.WHERE:
            self.match(token.WHERE)
            where = self.parse_value_declarations()
        else:    
            where = []
        return syntax.Definition(lhs=lhs, rhs=rhs, where=where,
                                 position=position)

    def parse_constructor_declarations(self):
        self.match(token.BEGIN)
        decls = []
        while self._token.type() == token.DELIM:
            self.match(token.DELIM)
            decls.append(self.parse_type_declaration())
        self.match(token.END)
        return decls

    def parse_id(self):
        name = self._token.value()
        if self.is_operator_part():
            self.fail('operator-part-is-not-a-variable', name=name)
        self.match(token.ID)
        if common.is_operator(name) and not self.is_declared_operator(name):
            self.fail('undeclared-operator', name=name)
        return name

    def parse_num(self):
        tok = self._token
        self.match(token.NUM)
        return tok.value()

    def parse_expression(self):
        if self._token.type() == token.FRESH:
            return self.parse_fresh()
        else:
            return self.parse_expression_mixfix()

    def parse_fresh(self):
        position = self.current_position()
        self.match(token.FRESH)
        ids = []
        while self._token.type() == token.ID:
            ids.append(self.parse_id())
        self.match(token.IN)
        body = self.parse_expression()
        return syntax.fresh_many(ids, body, position=position)

    def parse_expression_mixfix(self, level=0):
        if level == 0:
            level = self._prectable.first_level()
        if level is None:
            return self.parse_application()
        fixity = self._prectable.fixity(level)
        if fixity == token.INFIX:
            return self.parse_expression_infix(level)
        elif fixity == token.INFIXL:
            return self.parse_expression_infixl(level)
        elif fixity == token.INFIXR:
            return self.parse_expression_infixr(level)
        else:
            print(fixity)
            raise Exception('Fixity not implemented.')

    def parse_expression_infix(self, level):
        position = self.current_position()
        status = []
        children = []
        while not self.end_of_expression():
            tokval = self._token.value()
            must_read_part = (
              (len(status) == 0
               and self.is_operator_part()
               and self._prectable.is_status_prefix_in_level(level, [tokval]))
              or
              (len(status) > 0 and status[-1] == '')
            )
            if must_read_part:
                if not self.is_operator_part() or \
                   not self._prectable.is_status_prefix_in_level(
                         level,
                         status + [tokval]):
                    if len(status) == 1:
                        break
                    else:
                        self.fail('expected-operator-part',
                                  status=lexer.operator_from_parts(status))
                status.append(tokval)
                self.next_token()
            else:
                status.append('')
                next_level = self._prectable.next_level(level)
                children.append(self.parse_expression_mixfix(level=next_level))
            if self._prectable.is_status_in_level(level, status):
                expr = syntax.Variable(name=lexer.operator_from_parts(status))
                for arg in children:
                    expr = syntax.Application(fun=expr, arg=arg,
                                              position=position)
                return expr
        if status == ['']:
            return children[0]
        self.fail('cannot-parse-expression')

    def parse_expression_infixl(self, level):
        position = self.current_position()
        next_level = self._prectable.next_level(level)
        expr = self.parse_expression_mixfix(level=next_level)
        while self.is_operator_part() and \
              self._prectable.is_binop_in_level(level, self._token.value()):
            op = lexer.operator_from_parts(['', self._token.value(), ''])
            operator = syntax.Variable(name=op, position=position)
            self.next_token()
            arg = self.parse_expression_mixfix(level=next_level)
            expr = syntax.Application(
                     fun=syntax.Application(
                           fun=operator,
                           arg=expr,
                           position=position),
                     arg=arg,
                     position=position)
        return expr

    def parse_expression_infixr(self, level):
        position = self.current_position()
        next_level = self._prectable.next_level(level)
        expr = self.parse_expression_mixfix(level=next_level)
        if self.is_operator_part() and \
           self._prectable.is_binop_in_level(level, self._token.value()):
            op = lexer.operator_from_parts(['', self._token.value(), ''])
            operator = syntax.Variable(name=op, position=position)
            self.next_token()
            arg = self.parse_expression_mixfix(level=level)
            return syntax.Application(
                     fun=syntax.Application(
                           fun=operator,
                           arg=expr,
                           position=position),
                     arg=arg,
                     position=position)
        else:
            return expr

    def end_of_expression(self):
        return self._token.type() in [
            token.EQ,
            token.WHERE,
            token.DELIM,
            token.RPAREN,
            token.END,
        ]

    def parse_application(self):
        position = self.current_position()
        expr = self.parse_atom()
        while not self.end_of_application():
            arg = self.parse_atom()
            expr = syntax.Application(fun=expr, arg=arg, position=position)
        return expr

    def end_of_application(self):
        return self.is_operator_part() or self.end_of_expression()

    def parse_atom(self):
        position = self.current_position()
        if self._token.type() == token.LPAREN:
            self.match(token.LPAREN)
            if self._token.type() == token.RPAREN:
                self.match(token.RPAREN)
                return syntax.Variable(name="()", position=position)
            expr = self.parse_expression()
            self.match(token.RPAREN)
            return expr
        elif self._token.type() == token.NUM:
            return syntax.IntegerConstant(value=self.parse_num(),
                                          position=position)
        elif self._token.type() == token.ID:
            return syntax.Variable(name=self.parse_id(), position=position)
        elif self._token.type() == token.UNDERSCORE:
            self.next_token()
            v = syntax.fresh_variable(prefix='_', position=position)
            return syntax.Fresh(var=v.name, body=v, position=position)
        self.fail('expected-atom', got=self._token)

    ##

    def declare_operator(self, fixity, precedence, name, position=None):
        if position is None:
            position = self._token.position()
        self._prectable.declare_operator(fixity, precedence, name,
                                         position=position)

    def is_declared_operator(self, name):
        return self._prectable.is_declared_operator(name)

    def match(self, type):
        if self._token.type() == type:
            self.next_token() 
        else:
            self.fail('token-mismatch', expected=type, got=self._token)

    def match_any(self, types):
        if self._token.type() in types:
            self.next_token() 
        else:
            self.fail('token-mismatch', expected=types, got=self._token)

    def unshift(self, token):
        prev_token = self._token
        prev_stream = self._token_stream
        def g():
            yield prev_token
            yield from prev_stream
        self._token_stream = g()
        self._token = token

    def current_position(self):
        return self._token.position()

    def next_token(self):
        try:
            self._token = next(self._token_stream)
        except StopIteration:
            pass

    def is_operator_part(self):
        return self._token.type() == token.ID and \
               self._prectable.is_declared_part(self._token.value())

    def fail(self, msg, **args):
        raise common.LangException(
                'parser',
                msg,
                position=self._token.position(),
                **args
              )

if __name__ == '__main__':
    import sys

    parser = Parser(sys.stdin.read())
    print(syntax.pprint(parser.parse_program()))

