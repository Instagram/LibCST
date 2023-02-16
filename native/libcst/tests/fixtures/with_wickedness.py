# with_wickedness

with foo :
    pass

with foo, bar:
    pass

with (foo, bar):
    pass

with (foo, bar,):
    pass

with foo, bar as bar:
    pass

with (foo, bar as bar):
    pass

with (foo, bar as bar,):
    pass

async def f():
    async with foo:

        with bar:
            pass

    async with foo :
        pass

    async with foo, bar:
        pass

    async with (foo, bar):
        pass

    async with (foo, bar,):
        pass

    async with foo, bar as bar:
        pass

    async with (foo, bar as bar):
        pass

    async with (foo, bar as bar,):
        pass

    async  with  foo(1+1)  as  bar  , 1  as (a, b, ) , 2 as [a, b] , 3 as a[b]  :
        pass
