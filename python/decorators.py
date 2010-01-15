# taken from http://www.artima.com/weblogs/viewpost.jsp?thread=240808

def try1():

    class myDecorator(object):

        def __init__(self, f):
            print "inside myDecorator.__init__()"
            f() # Prove that function definition has completed

        def __call__(self):
            print "inside myDecorator.__call__()"

    @myDecorator
    def aFunction():
        print "inside aFunction()"

    print "Finished decorating aFunction()"

    aFunction()


def try2():

    class entryExit(object):

        def __init__(self, f):
            self.f = f

        def __call__(self):
            print "Entering", self.f.__name__
            self.f()
            print "Exited", self.f.__name__

    @entryExit
    def func1():
        print "inside func1()"

    @entryExit
    def func2():
        print "inside func2()"

    func1()
    func2()


def try3():
    def entryExit(f):
        def new_f():
            print "Entering", f.__name__
            f()
            print "Exited", f.__name__
        return new_f

    @entryExit
    def func1():
        print "inside func1()"

    @entryExit
    def func2():
        print "inside func2()"

    func1()
    func2()
    print func1.__name__
    print func2.__name__
