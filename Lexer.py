import ply.lex as lex

# List of token names.   This is always required
reserved = {
        'if': 'IF',
        'else': 'ELSE',
        'while': 'WHILE',
        'int': 'INT',
        'float': 'FLOAT',
        'bool': 'BOOL',
        'String': 'STRING',
        'dim': 'DIM',
        'sub': 'SUB',
        'input': 'INPUT',
        'do': 'DO',
        'print': 'PRINT',
        'for': 'FOR',
        'to': 'TO',
        'Program': 'PROGRAM',
        'goSub': 'GOSUB',
        'or': 'OR',
        'and': 'AND',
        'nor': 'NOR',
        'nand': 'NAND',
        'each': 'EACH',
        'in': 'IN',
        'switch': 'SWITCH',
        'case': 'CASE',
        'break': 'BREAK',
        'length': 'LENGTH',
        'True': 'TRUE',
        'False': 'FALSE',
        'as': 'AS',
        'let': 'LET'
    }

tokens = [
             'semi_colon',
             'shift_left',
             'shift_right',
             'plusplus',
             'minusminus',
             'id',
             'num',
             'plus',
             'minus',
             'times',
             'divide',
             'exponential',
             'mod',
             'plus_equal',
             'minus_equal',
             'times_equal',
             'divide_equal',
             'exponential_equal',
             'mod_equal',
             'parenthesis1',
             'parenthesis2',
             'cor1',
             'cor2',
             'scor1',
             'scor2',
             'coma',
             'EQU',
             'NEQ',
             'GTE',
             'LTE',
             'equal',
             'dos_puntos',
             'interrogacion',
             'str',
             'GT',
             'LT',
             'start_multcom',
             'end_multcom',
             'start_com',
             'letters',
         ] + list(reserved.values())


def my_lexer():
    t_ignore = ' \t'  # \r\n\f\v
    t_shift_left = r'<<'
    t_shift_right = r'>>'
    t_start_com = r'//'
    t_plusplus = r'\+\+'
    t_minusminus = r'\-\-'
    t_minus_equal = r'\-='
    t_times_equal = r'\*='
    t_divide_equal = r'/='
    t_plus_equal = r'\+='
    t_start_multcom = r'/\*'
    t_end_multcom = r'\*/'
    t_exponential_equal = r'\^='
    t_mod_equal = r'%='
    t_EQU = r'=='
    t_LTE = r'<='
    t_GTE = r'>='
    t_NEQ = r'!='
    t_GT = r'>'
    t_LT = r'<'
    t_equal = r'='
    t_minus = r'\-'
    t_times = r'\*'
    t_divide = r'/'
    t_plus = r'\+'
    t_exponential = r'\^'
    t_mod = r'%'
    t_interrogacion = r'\?'

    def t_newline(t):
        r'\n+'
        t.lexer.lineno += len(t.value)

    def t_semi_colon(t):
        r';'
        t.type = 'semi_colon'
        return t

    def t_coma(t):
        r'\,'
        t.type = 'coma'
        return t

    def t_parenthesis1(t):
        r'\('
        t.type = 'parenthesis1'
        return t

    def t_parenthesis2(t):
        r'\)'
        t.type = 'parenthesis2'
        return t

    def t_cor1(t):
        r'\{'
        t.type = 'cor1'
        return t

    def t_cor2(t):
        r'\}'
        t.type = 'cor2'
        return t

    def t_scor1(t):
        r'\['
        t.type = 'scor1'
        return t

    def t_scor2(t):
        r'\]'
        t.type = 'scor2'
        return t

    def t_dos_puntos(t):
        r'\:'
        t.type = 'dos_puntos'
        return t

    def t_id(t):
        r'[a-zA-Z_][a-zA-Z_0-9]*'
        t.type = reserved.get(t.value, 'id')
        return t

    def t_str(t):
        r'[\"\'][\[\'a-zA-Z_0-9\] @!#\-+=/\*?¿¡^%&():.;,\\]*[\"\']'
        t.type = 'str'
        return t

    def t_letters(t):
        r'[a-zA-Z_]'
        t.type = 'str'
        return t

    def t_tipo(t):
        r'[a-z_][a-zA-Z_0-9]*'
        t.type = 'tipo'
        return t

    def t_valor(t):
        r'[a-zA-Z_][a-zA-Z_0-9]*'
        t.type = reserved.get(t.value, 'valor')
        return t

    def t_num(t):
        r'\d+'
        t.value = int(t.value)
        t.type = 'num'
        return t

    def t_error(t):
        print("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)

    # Build the lexer from my environment and return it
    return lex.lex()