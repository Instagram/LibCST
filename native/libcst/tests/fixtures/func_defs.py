def f(a, /,): pass
def f(a, /, c, d, e): pass
def f(a, /, c, *, d, e): pass
def f(a, /, c, *, d, e, **kwargs): pass
def f(a=1, /,): pass
def f(a=1, /, b=2, c=4): pass
def f(a=1, /, b=2, *, c=4): pass
def f(a=1, /, b=2, *, c): pass
def f(a=1, /, b=2, *, c=4, **kwargs): pass
def f(a=1, /, b=2, *, c, **kwargs,): pass


def g(
    a,
    /,
):
    pass


def f(a, /, c, d, e):
    pass


def f(a, /, c, *, d, e):
    pass


def f(
    a,
    /,
    c,
    *,
    d,
    e,
    **kwargs,
):
    pass


def f(
    a=1,
    /,
):
    pass


def f(a=1, /, b=2, c=4):
    pass


def f(a=1, /, b=2, *, c=4):
    pass


def f(a=1, /, b=2, *, c):
    pass


def f(
    a=1,
    /,
    b=2,
    *,
    c=4,
    **kwargs,
):
    pass


def f(
    a=1,
    /,
    b=2,
    *,
    c,
    **kwargs,
):
    pass
