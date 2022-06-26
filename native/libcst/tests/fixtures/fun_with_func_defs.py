def f(a, /,): pass
def f(a, / ,): pass
def f(a, / ): pass
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

def foo(a, *
    , bar):
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

def say_hello(
    self, user: str, /
):
    print('Hello ' + user)


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


async   def  foo (
    bar  :  Baz ,
)   ->   zooooooooom  : ...


async def foo(bar  :  Baz  = 0 ) : ...

async def foo() -> Bar: ...

async def outer(
    foo
) -> Bar :
    def   inner(lol: Lol) -> None:
        async def core ():
            await lol
    def second(inner):
        pass

def stars (
    yes :  bool  =  True  ,
    /  ,
    noes   : List[bool]   =  [ * falses ],
    * all : The[Rest],
    but : Wait[Theres[More]] ,
    ** it : ends[now]   ,

)  -> ret:
    pass

def stars (
    yes :  bool  =  True  ,
    /  ,
    noes   : List[bool]   =  [ * falses ],
    * all : The[Rest],
    but : Wait[Theres[More]] ,
    ** it : ends[now[without_a_comma]]   

) -> ret  :
    pass


def foo(bar: (yield)) -> (yield): something: (yield another)

def foo( bar: (yield)) -> (yield) :
    something: (yield another)
    return 3  # no
    return  # yes


def f():
    for (yield 1)[1] in [1]:
        pass


@decorators
# foo
@woohoo
def f():
    pass

@getattr(None, '', lambda a: lambda b: a(b+1))
def f(): ...


@a(now_this = lol)
def f(): ...
