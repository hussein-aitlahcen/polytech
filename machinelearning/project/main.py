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
judge = lambda G, X, y: [(y[fst(ix)], max(map(partial(right, partial(flip, app, snd(ix))), enumerate(G)), key =snd))
                            for ix
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
  Execute the given classifier and returns the decisions made
'''
exec_classifier = lambda data: error(judge(learn(fst(fst(data)),
                                                 snd(fst(data))),
                                           fst(snd(data)),
                                           snd(snd(data))))
'''
  Execute multipe classifiers against the same set of sample, yielding their score (elapsed_time, error_rate)
'''
race  = lambda rawSample, rawUnknown, classifier: (fst(classifier),
                                                   bimap(lambda t: "elapsed: " + str(t),
                                                         lambda e: "error: " + str(e),
                                                         speed(lambda: exec_classifier(bimap(snd(classifier),
                                                                                             snd(classifier),
                                                                                             (rawSample,
                                                                                              rawUnknown))))))
'''
  Differents classifiers to test
'''
classifiers = [
    ("Bayesian + Gaussian", identity),
    ("PCA + Gaussian", partial(left, lambda X: PCA(n_components=10).fit_transform(X)))
]
for x in map(partial(race,
                     (np.load('./data/trn_img.npy'),
                      np.load('./data/trn_lbl.npy')),
                     (np.load('./data/dev_img.npy'),
                      np.load('./data/dev_lbl.npy'))),
             classifiers) :
    print(str(x))
