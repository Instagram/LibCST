var = 0


class C:
    var = 1

    if True:

        def f(self):
            var = 2
            list(var)
            enumerate(var)

    def g(self):
        C.var = 3


var = 4

x = C()
x.var = 5
