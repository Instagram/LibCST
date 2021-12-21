# with_wickedness

with foo : ...

async def f():
    async with foo as bar:

        with bar:
            pass

    async  with  foo(1+1)  as  bar  , 1  as (a, b, ) , 2 as [a, b] , 3 as a[b]  :
        pass

