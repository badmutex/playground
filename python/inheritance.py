
class Base1:
    def run(self):
        return "Base1"

class Base2:
    def run(self):
        return "Base2"

class Child(Base1, Base2):
    def run(self):
        b1 = Base1.run(self)
        b2 = Base2.run(self)
        return "%s\n%s\n%s" % (b1,b2,"Child")


c = Child()
print zc.run()

