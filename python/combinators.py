
def K (x):
    return lambda _: x

def S (x, y, z):
    return x(z)(y(z))

def I (x):
    return S(K, K, x)


def i(x):
    return x
