# fmt: off

type TA = int

type TA1[A] = lambda A: A

class Outer[A]:
    type TA1[A] = None

type TA1[A, B] = dict[A, B]

class Outer[A]:
    def inner[B](self):
        type TA1[C] = TA1[A, B] | int
        return TA1

def more_generic[T, *Ts, **P]():
    type TA[T2, *Ts2, **P2] = tuple[Callable[P, tuple[T, *Ts]], Callable[P2, tuple[T2, *Ts2]]]
    return TA

type Recursive = Recursive

def func[A](A): return A

class ClassA:
    def func[__A](self, __A): return __A

class ClassA[A, B](dict[A, B]):
    ...

class ClassA[A]:
    def funcB[B](self):
        class ClassC[C]:
            def funcD[D](self):
                return lambda: (A, B, C, D)
        return ClassC

class Child[T](Base[lambda: (int, outer_var, T)]): ...

type Alias[T: ([T for T in (T, [1])[1]], T)] = [T for T in T.__name__]
type Alias[T: [lambda: T for T in (T, [1])[1]]] = [lambda: T for T in T.__name__]

class Foo[T: Foo, U: (Foo, Foo)]:
    pass

def func[T](a: T = "a", *, b: T = "b"):
    return (a, b)

def func1[A: str, B: str | int, C: (int, str)]():
    return (A, B, C)

type   A [  T ,  *  V ]  =foo;type B=A

def AAAAAAAAAAAAAAAAAA [  T :  int  ,*Ts  ,  ** TT  ]  ():pass
class AAAAAAAAAAAAAAAAAA [  T :  int  ,*Ts  ,  ** TT  ] :pass

def yikes[A:int,*B,**C](*d:*tuple[A,*B,...])->A:pass

def func[T=int, **U=float, *V=None](): pass

class C[T=int, **U=float, *V=None]: pass

type Alias[T = int, **U = float, *V = None] = int

default = tuple[int, str]
type Alias[*Ts = *default] = Ts
type Foo[  * T = * default  ] = int
type Foo[*T=*default ]=int
type Foo [     *  T   =     *   default ] = int