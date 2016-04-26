import ply.yacc as yacc

# Get the token map from the lexer.  This is required.
from lex import tokens

DEBUG = True

# Namespace & built-in functions

name = {}
d_let = {}

def let_sub(l):
    if not isinstance(l, list):
        if l in d_let:
            return d_let[l]
        return l
    if len(l) == 1:
        return [let_sub(l[0])]
    return [let_sub( l[0] )] + let_sub( l[1:] )

def let(l):
    if len(l) == 1:
        return l[0][1]
    d_let[l[0][0]] = l[0][1]
    list_subbed = let_sub(l[1])
    res = 0
    if isinstance(list_subbed, list):
        res = lisp_eval(list_subbed[0], list_subbed[1:])
    else:
        res = list_subbed
    del d_let[l[0][0]]
    return res

name['let'] = let

def cons(l):
    return [l[0]] + l[1]

name['cons'] = cons

def concat(l):
    return l[0] + l[1]

name['concat'] = concat

def listar(l):
    return l

name['list'] = listar

def car(l):
    return l[0][0]

name['car'] = car

def cdr(l):
    return l[0][1:]

name['cdr'] = cdr

def eq(l):
    return l[0] == l[1]

name['eq'] = eq
name['='] = eq

def _and(l):
    return not False in l

name['and'] = _and

def _or(l):
    return True in l

name['or'] = _or

def cond(l):
    if l[0]:
        return l[1]

name['cond'] = cond

def add(l):
    return sum(l)

name['+'] = add
name['add'] = add

def minus(l):
    #Unary minus
    if len(l) == 1:
        return -l[0]
    if len(l) > 1:
        return l[0]- sum(l[1:])

name['-'] = minus
name['minus'] = minus

def multiply(l):
    if len(l) == 1:
        return l[0]
    if len(l) > 1:
        product = 1
        for i in l:
            product = product * i
    return product

name['*'] = multiply
name['multiply'] = multiply

def divide(l):
    if len(l) == 1:
        return l[0]
    if len(l) > 1:
        quotient = l[0]
        for i in l[1:]:
            if i == 0:
                return "undefined"
            quotient = quotient / i
    return quotient

name['/'] = divide
name['divide'] = divide

def _print(l):
    print lisp_str(l[0])

name['print'] = _print

#  Evaluation functions

def lisp_eval(simb, items):
    if simb in name:
        return call(simb, eval_lists(items))
    else:
        return [simb] + items

def call(simb, l):
    f = name[simb]
    try:
        return f(eval_lists(l))
    except TypeError:
        return [simb] + l

def eval_lists(l):
    r = []
    for i in l:
        if is_list(i):
            if i:
                r.append(lisp_eval(i[0], i[1:]))
            else:
                r.append(i)
        else:
            r.append(i)
    return r

# Utilities functions

def is_list(l):
    return type(l) == type([])

def lisp_str(l):
    if type(l) == type([]):
        if not l:
            return "()"
        r = "("
        for i in l[:-1]:
            r += lisp_str(i) + " "
        r += lisp_str(l[-1]) + ")"
        return r
    elif l is True:
        return "#t"
    elif l is False:
        return "#f"
    elif l is None:
        return 'nil'
    else:
        return str(l)

# BNF

def p_exp_atom(p):
    'exp : atom'
    p[0] = p[1]

def p_exp_qlist(p):
    'exp : quoted_list'
    p[0] = p[1]

def p_exp_call(p):
    'exp : call'
    p[0] = p[1]

def p_quoted_list(p):
    'quoted_list : QUOTE list'
    p[0] = p[2]

def p_list(p):
    'list : LPAREN items RPAREN'
    p[0] = p[2]

def p_items(p):
    'items : item items'
    p[0] = [p[1]] + p[2]

def p_items_empty(p):
    'items : empty'
    p[0] = []

def p_empty(p):
    'empty :'
    pass

def p_item_atom(p):
    'item : atom'
    p[0] = p[1]

def p_item_list(p):
    'item : list'
    p[0] = p[1]

def p_item_list(p):
    'item : quoted_list'
    p[0] = p[1]

def p_item_call(p):
    'item : call'
    p[0] = p[1]

def p_item_empty(p):
    'item : empty'
    p[0] = p[1]

'''
def p_let(p):
    'call : LPAREN LET list arg RPAREN'
    d_let[p[3][0]] = p[3][1]
    list_subbed = let_sub(p[4])
    if isinstance(list_subbed, list):
        p[0] = lisp_eval(list_subbed[0], list_subbed[1:])
    else:
        p[0] = list_subbed
    del d_let[p[3][0]]

def p_arg_atom(p):
    'arg : atom'
    p[0] = p[1]

def p_arg_list(p):
    'arg : list'
    p[0] = p[1]
'''

def p_call(p):
    'call : LPAREN SIMB items RPAREN'
    if DEBUG: print "Calling", p[2], "with", p[3]
    p[0] = lisp_eval(p[2], p[3])

def p_atom_simbol(p):
    'atom : SIMB'
    p[0] = p[1]

def p_atom_bool(p):
    'atom : bool'
    p[0] = p[1]

def p_atom_num(p):
    'atom : NUM'
    p[0] = p[1]

def p_atom_word(p):
    'atom : TEXT'
    p[0] = p[1]

def p_atom_empty(p):
    'atom :'
    pass

def p_true(p):
    'bool : TRUE'
    p[0] = True

def p_false(p):
    'bool : FALSE'
    p[0] = False

def p_nil(p):
    'atom : NIL'
    p[0] = None

# Error rule for syntax errors
def p_error(p):
    print "Syntax error!! ",p

# Build the parser
# Use this if you want to build the parser using SLR instead of LALR
# yacc.yacc(method="SLR")
yacc.yacc()


