
# Generic
ID = 'ID'
NUM = 'NUM'

# Invisible delimiters (for the offside rule)
BEGIN = 'BEGIN'
DELIM = 'DELIM'
END = 'END'
EOF = 'EOF'

# Symbols
LPAREN = 'LPAREN'
RPAREN = 'RPAREN'
COLON = 'COLON'

# Keywords
DATA = 'DATA'
EQ = 'EQ'
FRESH = 'FRESH'
IN = 'IN'
INFIX = 'INFIX'
INFIXL = 'INFIXL'
INFIXR = 'INFIXR'
UNDERSCORE = 'UNDERSCORE'
WHERE = 'WHERE'

class Token:

    def __init__(self, type, value, position):
        self._type = type
        self._value = value
        self._position = position

    def type(self):
        return self._type

    def value(self):
        return self._value

    def position(self):
        return self._position

    def __repr__(self):
        return '{type}("{value}")'.format(
                 type=self._type,
                 value=self._value,
               )

