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
from sklearn import preprocessing, svm

gi = lambda mu, C: (lambda sLogDet, invC: (lambda sign, logDet: lambda X: (lambda XminMu: -(sign * logDet) - XminMu.T @ invC @ XminMu)(X - mu))(fst(sLogDet), snd(sLogDet)))(np.linalg.slogdet(C), np.linalg.inv(C))

'''
  [X] = list of images
  [y] = expected classes for [X]
  f = zip
  g = groupby fst . sortby fst
  [y], [X] -f-> [(y, X)] -g-> [(y, [X])] -> [gi(mean([X]), cov([X]))]
'''
learn = lambda X, y: [gi(np.mean(subX, axis=0), np.cov(subX.T))
                      for (y, subX)
                      in map(partial(right, lambda x: np.array(list(map(snd, x)))),
                             groupby(sorted(zip(y, X), key=fst), fst))]
'''
  f = enumerate
  [g], [X], [y] -f-> [(i, X)] -> [(y[i], max(gi(X)))]
'''
judge = lambda G, X, y: [(y[i], max(map(partial(right, partial(flip, app, x)), enumerate(G)), key=snd))
                         for (i, x)
                         in enumerate(X)]
'''
  Given a function f, computes the execution time and return a tuple of the form: (elapsed, y)
  where y = f()
'''
speed = lambda f: left(lambda begin: time() - begin, (lambda begin: (begin, f()))(time()))
'''
  Compute the error rate
'''
error = lambda decisions: 1 - (len([1 for (expected, (computed, _)) in decisions if expected == computed]) / len(decisions))
'''
  Classify the given datas
'''
classify = lambda sample, unknown: (lambda X0, y0, X1, y1: judge(learn(X0, y0), X1, y1))(fst(sample), snd(sample), fst(unknown), snd(unknown))
'''
  Execute a classification with the given transformation applied against the set of sample, yielding the score (name, (elapsed_time, error_rate))
'''
bayesian = lambda rawSample, rawUnknown, name, transformation: (name, speed(lambda: (lambda transformed: error(classify(fst(transformed), snd(transformed))))(transformation((rawSample, rawUnknown)))))

vector = lambda rawSample, rawUnknown, name, transformation: (name, speed(lambda: error(list(map(lambda t: (snd(rawUnknown)[fst(t)], (snd(t), 0)), enumerate(svm.SVC().fit(fst(rawSample), snd(rawSample)).predict(fst(rawUnknown))))))))
'''
  Differents classifiers to test
'''
classifiers = [
    # ("Bayesian + Gaussian", (const(identity), (bayesian, [0]))),
    # ("PCA + Bayesian + Gaussian", (lambda x: lambda io: (lambda X0: (lambda fittedPCA: (lambda pipe: bimap(pipe, pipe, io))(partial(left, fittedPCA.transform)))(PCA(n_components=x).fit(X0)))(fst(fst(io))), (bayesian, range(49, 52))))
    ("SVM", (lambda x: lambda io: (lambda X0: (lambda fittedPCA: (lambda pipe: bimap(pipe, pipe, io))(compose(preprocessing.scale, partial(left, fittedPCA.transform))))(PCA(n_components=x).fit(X0)))(fst(fst(io))), (vector, range(50, 51))))
]

sample = (np.load('./data/trn_img.npy')[:4000],
          np.load('./data/trn_lbl.npy')[:4000])
unknown = (np.load('./data/dev_img.npy')[:4000],
           np.load('./data/dev_lbl.npy')[:4000])
result = speed(lambda: map(partial(right, partial(map, partial(right, snd))),
                           groupby(sorted([(variation, f(sample, unknown, name, transformation(variation)))
                                           for (name, (transformation, (f, variations)))
                                           in classifiers
                                           for variation
                                           in variations],
                                          key=compose(fst, snd)),
                           compose(fst, snd))))
print("################")
print("total time: " + str(fst(result)))
for x in snd(result):
    print("############")
    print(str(fst(x)))
    print("------------")
    for y in snd(x):
        print(str(y))
