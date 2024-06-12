
# foo

match  (  foo )   :  #comment

# more comments
    case  False  :  # comment

      ...
    case  (  True )  : ...
    case _ : ...
    case ( _  ) :  ... # foo

# bar

match x:
    case  "StringMatchValue"  : pass
    case   [1, 2]   : pass
    case   [  1 ,   * foo  , *  _ , ]: pass
    case  [  [ _,  ] , *_ ]: pass
    case  {1: _, 2: _}: pass
    case {  "foo" :  bar ,  ** rest }  : pass
    case { 1 : {**rest} ,  } :  pass
    case Point2D(): pass
    case  Cls (  0 , ) : pass
    case Cls  ( x=0,  y =  2)  :pass
    case  Cls  (  0  ,  1  ,  x  =  0  ,  y  =  2  )  :  pass
    case [x] as y: pass
    case  [x]   as  y    :  pass
    case (True)as x:pass
    case Foo:pass
    case (Foo):pass
    case (  Foo   ) :  pass
    case [  ( Foo  )   , ]: pass
    case Foo|Bar|Baz : pass
    case Foo  |    Bar |  (  Baz):  pass
    case x,y  ,  * more   :pass
    case y.z: pass
    case 1, 2: pass
    case ( Foo  (   )    ) : pass
    case (lol)  if (  True , )  :pass

