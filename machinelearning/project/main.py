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
import matplotlib.pyplot as plot
from common import *
from time import time
from math import log
from itertools import groupby
from functools import partial
from sklearn.decomposition import PCA

gi    = lambda mu, C: partial(lambda sLogDet, invC, X: (lambda XminMu: -(fst(sLogDet) * snd(sLogDet)) - XminMu.T @ invC @ XminMu)(X - mu),
                              np.linalg.slogdet(C),
                              np.linalg.inv(C))
'''
  [X] = list of images
  [y] = expected classes for [X]
  f = zip
  g = groupby fst . sortby fst
  [y], [X] -f-> [(y, X)] -g-> [(y, [X])] -> [gi(mean([X]), cov([X]))]
'''
learn = lambda X, y: [gi(np.mean(subX, axis =0), np.cov(subX.T))
                         for (y, subX)
                         in map(partial(right, lambda x: np.array(list(map(snd, x)))),
                                groupby(sorted(zip(y, X), key=fst), fst))]
'''
  f = enumerate
  [X] -f-> [(i, X)] -> [(y[i], max(gi(X)))]
'''
judge = lambda G, X, y: [(y[i], max(map(partial(right, partial(flip, app, x)), enumerate(G)), key =snd))
                         for (i, x)
                         in enumerate(X)]
'''
  Given a function f, computes the time and return a tuple of the form: (elapsed, y)
  where y = f()
'''
speed = lambda f: left(lambda begin: time() - begin, (lambda begin: (begin, f()))(time()))
'''
  Compute the error rate
'''
error = lambda decisions: 1 - (len([decision for decision in decisions if fst(decision) == fst(snd(decision))]) / len(decisions))
'''
  Classify the given datas
'''
classify = lambda data: error(judge(learn(fst(fst(data)),
                                                 snd(fst(data))),
                                           fst(snd(data)),
                                           snd(snd(data))))
'''
  Execute multipe classifiers against the same set of sample, yielding their score (name, (elapsed_time, error_rate))
'''
race  = lambda rawSample, rawUnknown, name, transformation: (name,
                                                             speed(lambda: classify(bimap(transformation,
                                                                                          transformation,
                                                                                          (rawSample,
                                                                                           rawUnknown)))))
'''
  Differents classifiers to test
'''
classifiers = [
    ("Bayesian + Gaussian", (lambda x: identity, [0])),
    ("PCA + Gaussian", (lambda x: partial(left, lambda X: PCA(n_components=x).fit_transform(X)), [10, 40, 100, 200, 400]))
]

sample = (np.load('./data/trn_img.npy'),
          np.load('./data/trn_lbl.npy'))

unknown = (np.load('./data/dev_img.npy'),
           np.load('./data/dev_lbl.npy'))

result = map(partial(right, partial(map, partial(right, snd))),
             groupby(sorted([(variation, race(sample, unknown, name, transformation(variation)))
                             for (name, (transformation, variations))
                             in classifiers
                             for variation
                             in variations],
                            key=compose(fst, snd)),
                     compose(fst, snd)))

for x in result:
    print("############")
    print(str(fst(x)))
    for y in snd(x):
        print(str(y))
