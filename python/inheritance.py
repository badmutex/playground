
class Base1:
    def __init__(self):
        self.var = 'b1!'

    def run(self):
        return "Base1 %s" % (self.var)

class Base2:
    def __init__(self):
        self.var = 'b2!'

    def run(self):
        return "Base2 %s" % (self.var)

class Child(Base1, Base2):
    def __init__(self):
        self.var = 'c!'

    def run(self):
        b1 = Base1.run(self)
        b2 = Base2.run(self)
        return "%s\n%s\n%s %s" % (b1,b2,"Child", self.var)


zc = Child()
print zc.run()

