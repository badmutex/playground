


def returns(v):
    return lambda s: (s,v)

def bind(m, f):
    def binder(s0):
        s1, v = m(s0)
        return f(v)(s1)
    return binder

def bindig(ma, mb):
    return bind(ma, lambda _: mb)

def gets(s):
    return (s,s)

def puts(s):
    return lambda _: (s, None)

def mods(f):
    return lambda s: (f(s), None)



incs = mods(lambda v: v+1)


