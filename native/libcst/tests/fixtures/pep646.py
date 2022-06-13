# see https://github.com/python/cpython/pull/31018/files#diff-3f516b60719dd445d33225e4f316b36e85c9c51a843a0147349d11a005c55937

A[*b]
A[  *  b  ]
A[ *  b ,  ]
A[*b] = 1
del A[*b]

A[* b ,  * b]
A[ b, *b]
A[* b, b]
A[ *  b,b, b]
A[b, *b, b]

A[*A[b, *b, b], b]
A[b, ...]
A[*A[b, ...]]

A[ * ( 1,2,3)]
A[ * [ 1,2,3]]

A[1:2, *t]
A[1:, *t, 1:2]
A[:, *t, :]
A[*t, :, *t]

A[* returns_list()]
A[*returns_list(), * returns_list(), b]

def f1(*args: *b): pass
def f2(*args: *b, arg1): pass
def f3(*args: *b, arg1: int): pass
def f4(*args: *b, arg1: int = 1): pass

def f(*args: *tuple[int, ...]): pass
def f(*args: *tuple[int, *Ts]): pass
def f() -> tuple[int, *tuple[int, ...]]: pass