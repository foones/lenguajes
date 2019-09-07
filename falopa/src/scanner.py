
class Scanner:

    def __init__(self, source='', filename='...', index=0, line=1, col=0):
        self._source = source
        self._fn = filename
        self._i = index
        self._line = line
        self._col = col

    def line(self):
        return self._line

    def col(self):
        return self._col

    def eof(self):
        return self._i >= len(self._source)

    def peek(self):
        return self._source[self._i]

    def next(self):
        assert not self.eof()
        if self._source[self._i] == '\n':
            line = self._line + 1
            col = 0
        else:
            line = self._line
            col = self._col + 1
        return Scanner(self._source, self._fn, self._i + 1, line, col)

    def match(self, string):
        return self._source[self._i : self._i + len(string)] == string

    def consume(self, string):
        assert self.match(string)
        s = self
        for c in string:
            s = s.next()
        return s

    def __repr__(self):
        return '{filename}:{line}:{col}'.format(
                 filename=self._fn,
                 line=self._line,
                 col=self._col,
               )

