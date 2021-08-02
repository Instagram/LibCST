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
