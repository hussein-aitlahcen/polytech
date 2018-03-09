# common.py ---

# Copyright (C) 2018 Hussein Ait-Lahcen

# Author: Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

from functools import reduce

# identity
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

# chain a list of functions by composing them, order matters
chain = lambda fs: reduce(compose, fs, identity)

# function composition
compose = lambda g, f: lambda x: g(f(x))

