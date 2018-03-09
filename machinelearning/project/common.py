compose = lambda g, f, x: g(f(x))

# Identity
identity = lambda x: x

''' left/right projection of product type

 fst: (a x b) -> a
         t    -> a
 snd: (a x b) -> b
         t    -> b

'''
fst      = lambda t: t[0]
snd      = lambda t: t[1]

''' function evaluation, useful for partial application

 (a -> b) -> a -> b
     f    -> x -> y

'''
app      = lambda f, x: f(x)

''' swap function arguments

 (a -> b -> c) -> b -> a -> c
       f       -> x -> y -> z

'''
flip     = lambda f, x, y: f(y, x)

''' project and reconstruct product type with given functions

 (a -> c) -> (b -> d) -> (a x b) -> (c x d)
    f     ->    g     ->    t    ->    u

'''
bimap    = lambda f, g, t: (f(fst(t)), g(snd(t)))

# obvious helpers for bimap
left     = lambda f, t: bimap(f, identity, t)
right    = lambda f, t: bimap(identity, f, t)

