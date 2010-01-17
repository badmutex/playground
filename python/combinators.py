
def K (x):
    return lambda _: lambda c: c(x)

def S (x):
    return lambda y: lambda z: lambda c: c(x(z)(y(z)))

def I (x):
    return lambda c: c(S(K)(K)(x) (ident)(ident) )


def i(x):
    return x

def ident(x): return x


def test():
    print 'K(42)(24) = ' ,  K(42)(24)(ident)
    print 'i(42) = ' , i(42)
    print 'I(42) = ' , I(42)(ident)

if __name__ == '__main__':
    test()
