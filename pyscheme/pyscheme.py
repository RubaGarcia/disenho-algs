# scheme.py
#
# Challenge: Can you implement a scheme interpreter in Python that's
# capable of executing the following procedure?

# A procedure definition for:
#
#   (define fact
#      (lambda (n) (if (= n 1)
#                   1
#                   (* n (fact (- n 1))))))
#
# It's represented in Python using the following tuple:

fact = ('define', 'fact', 
        ('lambda', ('n',), ('if', ('=', 'n', 1),
                            1,
                            ('*', 'n', ('fact', ('-', 'n', 1))))))



def pon_en_env(x, y):
    global env
    env[x] = seval(y)


env = {'+': lambda x, y: x+y,
       '-': lambda x,y: x-y,
       '*': lambda x,y: x*y,
       '=': lambda x, y: x==y
    }

def hacer_funcion(argumentos, cuerpox):
    def funcion(*valores):
        cuerpo = cuerpox
        for nombre, valor in zip(argumentos, valores):
            cuerpo = substitucion(cuerpo, nombre, valor)
        return cuerpo
    return funcion

def substitucion(exp, nombre, valor):
    if exp == nombre:
        return valor
    elif isinstance(exp, tuple):
        return tuple(substitucion(e, nombre, valor) for e in exp)
    else:
        return exp

# You will define the following procedure for evaluating an expression
def seval(sexp):
    if isinstance(sexp, int):
        return sexp
    elif isinstance(sexp, str):
        return env.get(sexp, sexp) 
    elif isinstance(sexp, tuple):
        if sexp[0] == 'if':
            return seval(sexp[2]) if seval(sexp[1]) else seval(sexp[3])
        elif sexp[0] == 'lambda':
            return hacer_funcion(sexp[1], sexp[2])
        elif sexp[0] == 'define':
            env.update({''+sexp[1]:seval(sexp[2])})
            return env.get(sexp[1])
        func = seval(sexp[0])
        print(func)
        args = [seval(e) for e in sexp[1:]]
        return func(*args)
    
    

# In writing seval, you are ONLY allowed to use the rules of Scheme
# evaluation that you currently know about.  So far, this includes the
# substitution model and the notion of special forms.
    
# Some basic tests
assert seval(42) == 42
assert seval(('+', 2, 3)) == 5
seval(('define', 'n', 5))
assert seval('n') == 5

# Now the ultimate test--can you run your procedure?
seval(fact)
print(seval(seval(('fact', 'n'))))
assert seval(('fact', 'n')) == 120
