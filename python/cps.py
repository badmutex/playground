

def hello():
    return lambda f: f('hello world!')


def caps(s):
    return lambda f: f(s.upper())


def printf(s):
    print s

def ident(i): return i


def test():
    cap = hello()(caps)
    cap(printf)
    return cap(ident)


if __name__ == '__main__':
    test()
