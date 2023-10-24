# scheme.py
# Core-idea: An Evaluator

# An Environment that stores names/values (global environment)
env = {
   'two': 2,
   '+': lambda x, y: x+y,     # "Built-in" procedure
   '-': lambda x, y: x-y,
   '*': lambda x, y: x*y,
   '=': lambda x, y: x == y,
   '<': lambda x, y: x < y,
}

def substitucion(exp, name, value):
    if exp == name:
        return value
    elif isinstance(exp, tuple):
        return tuple(substitucion(e, name, value) for e in exp)
    else:
        return exp

def crear_funcion(argnames, lexp, ambiente):
    # env is the environment where the procedure was *DEFINED*
    def llamar(*values):      # *args take any arguments
        assert len(values) == len(argnames), "Wrong # args"
        new_ambiente = dict()
        new_ambiente["$padre"] = ambiente
        exp = lexp    # Starting expression
        for name, val in zip(argnames, values):
            new_ambiente[name] = values
        return seval(exp, new_ambiente)
    return llamar


def seval(exp, ambiente):
    if isinstance(exp, (int, bool)):
        return exp
    elif isinstance(exp, str):
        while exp not in ambiente:
            ambiente = ambiente["$padre"]
        return ambiente[exp]
    elif isinstance(exp, tuple):
        if exp[0] == 'define':     # "Special Form"
            name = exp[1]
            value = seval(exp[2],ambiente)
            env[name] = value
            return
        elif exp[0] == 'if':
            test = seval(exp[1], ambiente)
            if test:
                return seval(exp[2], ambiente)
            else:
                return seval(exp[3], ambiente)
        elif exp[0] == 'lambda':
            argnames = exp[1]
            lexp = exp[2]
            return crear_funcion(argnames, lexp, ambiente)
        elif exp[0] == 'quote':
            return exp[1]

        # Tuples are procedure calls
        # Evaluate all of the parts first
        evaluated_exp = [seval(e, ambiente) for e in exp]
        print(evaluated_exp)
        return evaluated_exp[0](*evaluated_exp[1:])


assert seval(42, env) == 42
assert seval("two", env) == 2
assert seval(('+', 2, 3), env) == 5
seval(('define', 'x', ('+', 2, 3)), env)
assert seval('x', env) == 5
assert seval(('if', ('<', 'x', 10), 3, 4), env) == 3

# Can you compute factorials?
# (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
seval(('define', 'a', 5), env)
seval(('define', 'fact', 
       ('lambda', ('n',), ('if', 
                           ('=', 'n', 0),
                           1,
                           ('*', 'n', ('fact', ('-', 'n', 1)))))), env)

assert seval(('fact', 'a'), env) == 120
