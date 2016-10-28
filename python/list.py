
class List:
    def __init__(self, val=None):
        """
        a -> [a]
        """
        self.head = val
        self.tail = None

    def __repr__(self):
        return 'cons(%r, %r)' % (self.head, self.tail)
        
def cons(x,xs):
    """
    a -> [a] -> [a]
    """
    ys = List(x)
    ys.tail = xs
    return ys

def append(x,xs):
    """
    a -> [a] -> [a]
    """
    if not xs.tail: return cons(xs.head, List(x))
    else:           return cons(xs.head, append(x,xs.tail))

def reverse(xs):
    """
    [a] -> [a]
    """
    if not xs.tail: return List(xs.head)
    else:           return append(xs.head, reverse(xs.tail))

def zipWith(f, xs,ys):
    """
    (a -> b -> c) -> [a] -> [b] -> [c]
    """
    if xs and ys: return cons(f(xs.head, ys.head), zip(xs.tail,ys.tail))
    else: return None

def zip(xs, ys):
    """
    [a] -> [b] -> [(a,b)]
    """
    return zipWith(lambda x,y: (x,y), xs, ys)

def length(xs):
    """
    [a] -> Int
    """
    if not xs: return 0
    else: return 1 + length(xs.tail)

def all(xs):
    """
    [Boolean] -> Boolean
    """
    if xs: return xs.head and all(xs.tail)
    else: return True


def eq(xs,ys):
    """
    [a] -> [b] -> Boolean
    """
    if length(xs) == length(ys):
        truths = zipWith(lambda x,y: x == y, xs, ys)
        return all(truths)
    else: return False


def Map(f,xs):
    """
    (a -> b) -> [a] -> [b]
    """
    if not xs: return None
    else:      return cons(f(xs.head), Map(f,xs.tail))


def t():
    l = cons(4,cons(3,cons(2,cons(1,None))))
    return eq(l,l)
    
def test():
    import doctest
    doctest.testfile('list.txt', verbose=True)

 
