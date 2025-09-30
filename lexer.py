import ply.lex as lex

# 1) Palabras reservadas
reserved = {
    'if'      : 'IF',
    'else'    : 'ELSE',
    'while'   : 'WHILE',
    'for'     : 'FOR',
    'return'  : 'RETURN',
    'break'   : 'BREAK',
    'continue': 'CONTINUE',
    'def'     : 'DEF',
    'class'   : 'CLASS',
    'int'     : 'INT',
    'float'   : 'FLOAT',
    'string'  : 'STRING_TYPE',
    'bool'    : 'BOOL',
    'true'    : 'TRUE',
    'false'   : 'FALSE',
    'null'    : 'NULL'
}

# 2) Lista de tokens
tokens = (
    'NUMBER',
    'ID',
    'STRING',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'POWER', 'MOD',
    'LPAREN', 'RPAREN',
    'COMMA', 'EQUALS',
) + tuple(reserved.values())

# 3) Tokens sencillos (regex directo)
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_POWER = r'\^'
t_MOD = r'%'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_COMMA = r','
t_EQUALS = r'='

# 4) Tokens complejos: números, identificadores, strings
def t_NUMBER(t):
    r'\d+(\.\d+)?([eE][+-]?\d+)?' #? Desarrollar por qué esta expresión

    # Convertir a float o int según corresponda
    if('.' in t.value) or ('e' in t.value) or ('E' in t.value):
        try:
            t.value = float(t.value)
        except ValueError:
            t.value = float(0.0) #? Fallback seguro?
    else:
        try:
            t.value = int(t.value)
        except ValueError:
            t.value = int(0)
    
    return t

def t_ID(t):
    r'[A-Za-z_][A-Za-z0-9_]*'
    t.type = reserved.get(t.value, 'ID') #? Cómo chequea esto?
    return t

def t_STRING(t):
    r'(\"([^\\\n]|(\\.))*?\")|(\'([^\\\n]|(\\.))*?\')'
    raw = t.value[1:-1]
    return t

# 5) Ignorar comentarios
def t_COMMENT(t):
    r'\#.*'
    pass

# 6) Ignorar espacios y tabulaciones
t_ignore = ' \t'

# 7) Manejo de saltos de línea
def t_newline(t):
    r'(\r\n|\r|\n)+'
    t.lexer.lineno += t.value.count('\n')

# 8) Función auxiliar para calcular la columna
def find_column(input_text, lexpos): # ? Cómo funciona esta función
    last_cr = input_text.rfind('\n', 0, lexpos)
    if last_cr < 0:
        return lexpos + 1
    else:
        return lexpos - last_cr
    
# 9) Manejo de errores
def t_error(t):
    col = find_column(t.lexer.lexdata, t.lexpos)
    print(f"Error léxico: carácter ilegal {t.value[0]!r} en línea {t.lineno}, columna [col]")
    
    start = t.lexer.lexdata.rfind('\n', 0, t.lexpos) + 1
    end = t.lexer.lexdata.find('\n', t.lexpos)
    if end == -1:
        end = len(t.lexer.lexdata)
    line_str = t.lexer.lexdata[start:end]
    pointer = ' ' * (col - 1) + '^'
    print(line_str)
    print(pointer)
    t.lexer.skip(1)

# 10) Construcción del lexer
lexer = lex.lex()

# 11) Función utilitaria para probar tokens
def tokenize(input_text):
    lexer.input(input_text)
    tokens_out = []
    while True:
        tok = lexer.token()
        if not tok:
            break
        col = find_column(lexer.lexdata, tok.lexpos)
        tokens_out.append({
            'type': tok.type,
            'value': tok.value,
            'lineno': tok.lineno,
            'col': col,
            'lexpos': tok.lexpos
        })

    return tokens_out

# 11) Prueba
if __name__ == '__main__':
    sample = (
        "int x = 3 + 4.5 * (y - 2)\n"
        "if x > 3 {\n"
        "   return x\n"
        "}\n"
        "# comentario\n"
        "string s = \"hola\\nmundo\"\n"
        "bad@char\n"
        "while true false null\n"
    )

for tok in tokenize(sample):
    print(tok)