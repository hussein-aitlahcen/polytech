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
from common import *
from time import time
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

def race(brainMaterial, judgable, classifier):
    (name, f) = classifier
    (X0, y0) = f(brainMaterial)
    (X1, y1) = f(judgable)
    begin = time()
    G = learn(X0, y0)
    decisions = judge(G, X1, y1)
    end = time()
    elapsed = end - begin
    error = 1 - (len([decision
                      for decision
                      in decisions
                      if fst(decision) == fst(snd(decision))]) / len(decisions))
    return name + ': elapsed=' + str(elapsed) + ', error=' + str(error)

'''
  [X] = list of images
  [y] = expected classes for [X]
  f = zip
  g = groupby fst . sortby fst
  [y], [X] -f-> [(y, X)] -g-> [(y, [X])] -> [gi(mean([X]), cov([X]))]
'''
learn    = lambda X, y: [gi(np.mean(subX, axis=0), np.cov(subX.T))
                         for (y, subX)
                         in map(partial(right, lambda x: np.array(list(map(snd, x)))),
                                groupby(sorted(zip(y, X), key=fst), fst))]
'''
  f = enumerate
  [X] -f-> [(i, X)] -> [(y[i], max(gi(X)))]
'''
judge    = lambda G, X, y: [(y[fst(ix)], max(map(partial(right, partial(flip, app, snd(ix))), enumerate(G)), key=snd))
                            for ix
                            in enumerate(X)]

bayesian = identity
pca = partial(left, lambda X: PCA(n_components=10).fit_transform(X))

classifiers = [
    ("Bayesian + Gaussian", bayesian),
    ("PCA + Gaussian", pca)
]

for x in map(partial(race,
                     (np.load('./data/trn_img.npy'),
                      np.load('./data/trn_lbl.npy')),
                     (np.load('./data/dev_img.npy'),
                      np.load('./data/dev_lbl.npy'))),
             classifiers) :
    print(x)
