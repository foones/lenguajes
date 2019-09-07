import common
import scanner
import token

SYMBOLS = {
    '(': token.LPAREN,
    ')': token.RPAREN,
}

KEYWORDS = {
    'data': token.DATA,
    'fresh': token.FRESH,
    'in': token.IN,
    'infix': token.INFIX,
    'infixl': token.INFIXL,
    'infixr': token.INFIXR,
    'where': token.WHERE,
    ':': token.COLON,
    '=': token.EQ,
    '_': token.UNDERSCORE,
}

OFFSIDE_KEYWORDS = {
    token.WHERE,
}

def operator_to_parts(name):
    parts = name.strip('_').split('_')
    result = []
    if name[0] == '_':
        result.append('')
    result.append(parts.pop(0))
    while len(parts) > 0:
        result.append('')
        result.append(parts.pop(0))
    if name[-1] == '_':
        result.append('')
    return result

def operator_from_parts(name):
    return ''.join([('_' if part == '' else part) for part in name])

class Lexer:

    def __init__(self, source, filename='...'):
        self._scanner = scanner.Scanner(source, filename)

    def tokens(self):
        "Yields a sequence of tokens, after applying the offside rule."
        col_stack = [0]
        previous_line = 0
        previous_offside = False
        tokens = self.raw_tokens()
        yield self.token(token.BEGIN, '')
        for tok in tokens:
            current_col = tok.position().col()
            current_line = tok.position().line()
            if previous_offside:
                if len(col_stack) > 0 and current_col <= col_stack[-1]:
                    yield self.token(token.END, '')
                else:
                    col_stack.append(current_col)
            if previous_offside or previous_line < current_line:
                while len(col_stack) > 0 and current_col < col_stack[-1]:
                    col_stack.pop()
                    yield self.token(token.END, '')
                if len(col_stack) > 0 and current_col == col_stack[-1]:
                    yield self.token(token.DELIM, '')
            previous_line = current_line
            yield tok
            if tok.type() in OFFSIDE_KEYWORDS:
                yield self.token(token.BEGIN, '')
                previous_offside = True
            else:
                previous_offside = False
        while len(col_stack) > 0:
            col_stack.pop()
            yield self.token(token.END, '')
        yield self.token(token.EOF, '')

    def raw_tokens(self):
        "Yields a sequence of tokens, before applying the offside rule."
        while not self.eof():
            yield self.next_token()

    def next_token(self):
        self.ignore_whitespace_and_comments()
        tok = self.match_symbol()
        if tok is not None:
            return tok
        elif self.is_ident(self.peek()):
            return self.match_name()
        else:
            self.fail('invalid-character', character=ord(self._scanner.peek()))

    def match_symbol(self):
        for symbol, type in SYMBOLS.items():
            if self.match(symbol):
                tok = self.token(type, symbol)
                self.consume(symbol)
                return tok
        return None

    def match_name(self):
        pos = self._scanner
        parts = []
        while not self._scanner.eof() and self.is_ident(self.peek()):
            if len(parts) > 0 and parts[-1] == '_' and self.peek() == '_':
                self.fail('consecutive-underscores')
            parts.append(self.peek())
            self.next()
        name = ''.join(parts)
        if common.is_number(name):
            return token.Token(token.NUM, int(name), position=pos)
        else:
            type = KEYWORDS.get(name, token.ID)
            return token.Token(type, name, position=pos)

    def eof(self):
        self.ignore_whitespace_and_comments()
        return self._scanner.eof()

    def peek(self):
        return self._scanner.peek()

    def next(self):
        self._scanner = self._scanner.next()

    def match(self, string):
        return self._scanner.match(string)

    def consume(self, string):
        self._scanner = self._scanner.consume(string)

    def token(self, type, value):
        return token.Token(type, value, position=self._scanner)

    def ignore_whitespace_and_comments(self):
        while True:
            self.ignore_whitespace()
            if self.match('--'):
                self.ignore_single_line_comment()
            elif self.match('{-'):
                self.ignore_multiline_comment()
            else:
                break

    def ignore_single_line_comment(self):
        while not self._scanner.eof() and self.peek() != '\n':
            self.next()

    def ignore_multiline_comment(self):
        b = 1
        self.next()
        while not self._scanner.eof() and b > 0:
            if self.match('{-'):
                b += 1
            elif self.match('-}'):
                self.next()
                b -= 1
            self.next()
        if self._scanner.eof() and b > 0:
            self.fail('unclosed-multiline-comment')

    def ignore_whitespace(self):
        while not self._scanner.eof() and common.is_blank(self.peek()):
            if self.peek() == '\t':
                self.fail('no-tabs-allowed')
            self.next()

    def is_ident(self, c):
        return common.is_readable(c) and c not in SYMBOLS

    def fail(self, msg, **args):
        raise common.LangException(
                'lexer',
                msg,
                position=self._scanner,
                **args
              )

if __name__ == '__main__':
    import sys

    lexer = Lexer(sys.stdin.read())
    for tok in lexer.tokens():
        print(tok)

