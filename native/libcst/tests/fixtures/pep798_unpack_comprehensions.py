
lists = [[1, 2], [3, 4]]
sets = [{1, 2}, {2, 3}]

result = [*L for L in lists]
result = [  *  L  for  L  in  lists  ]
result = [*range(3) for _ in range(3)]

result = {*s for s in sets}
result = {  *  s  for  s  in  sets  }

result = (*L for L in lists)
result = (  *  L  for  L  in  lists  )
