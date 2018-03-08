# main.py ---

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

import numpy as np
from math import log
from itertools import groupby
from functools import partial
from sklearn.decomposition import PCA

def gi(mu, C):
    (sign, logDetC) = np.linalg.slogdet(C)
    invC = np.linalg.inv(C)
    def f(X):
        XminMu = X - mu
        return  - (sign * logDetC) - XminMu.T @ invC @ XminMu
    return f

identity = lambda x: x

# left/right projection of product type
# fst: (a x b) -> a
#         t    -> a
# snd: (a x b) -> b
#         t    -> b
fst      = lambda t: t[0]
snd      = lambda t: t[1]

# function evaluation, useful for partial application
# (a -> b) -> a -> b
#     f    -> x -> y
# e.g. fix x and apply f, g, h...
app      = lambda f, x: f(x)

# swap function arguments
# (a -> b -> c) -> b -> a -> c
#       f          x -> y -> z
flip     = lambda f, x, y: f(y, x)

# project and reconstruct product type with given functions
# (a -> c) -> (b -> d) -> (a x b) -> (c x d)
#    f     ->    g     ->    t    ->    u
bimap    = lambda f, g, t: (f(fst(t)), g(snd(t)))

# obvious helpers for bimap
left     = lambda f, t: bimap(f, identity, t)
right    = lambda f, t: bimap(identity, f, t)

'''
  [X] = list of images
  [y] = expected classes for [X]
  f = zip
  g = groupby fst . sortby fst
  [y], [X] -f-> [(y, X)] -g-> [(y, [X])] -> [gi(mean([X]), cov([X]))]
'''
learn    = lambda X, y: [gi(np.mean(subX, axis=0), np.cov(subX.T)) for (y, subX) in map(partial(right, lambda x: np.array(list(map(snd, x)))), groupby(sorted(zip(y, X), key=fst), fst))]
'''
  f = enumerate
  [X] -f-> [(i, X)] -> [(y[i], max(gi(X)))]
'''
judge    = lambda X, y: [(y[fst(ix)], max(map(partial(right, partial(flip, app, snd(ix))), enumerate(G)), key=snd)) for ix in enumerate(X)]

G = learn(np.load('./data/trn_img.npy'), np.load('./data/trn_lbl.npy'))

decisions = judge(np.load('./data/dev_img.npy'), np.load('./data/dev_lbl.npy'))

error = 1 - (len([decision for decision in decisions if fst(decision) == fst(snd(decision))]) / len(decisions))

# pourcentage d'erreur sur les jugements
print(error)
