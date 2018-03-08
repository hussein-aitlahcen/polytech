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
# left/right projection of product type: (a x b) -> a, (a x b) -> b
fst      = lambda t: t[0]
snd      = lambda t: t[1]
# function evaluation, useful for partial application: (a -> b) -> a -> b
app      = lambda f, x: f(x)
# swap function arguments: (a -> b -> c) -> (b -> a -> c)
flip     = lambda f, x, y: f(y, x)
# project and reconstruct product type with given functions
# (a x b) -> (a -> c) -> (b -> d) -> (c x d)
bimap    = lambda f, g, t: (f(fst(t)), g(snd(t)))
# obvious helper for bimap
left     = lambda f, t: bimap(f, identity, t)
right    = lambda f, t: bimap(identity, f, t)
'
  [X] = list of images
  [y] = expected classes for [X]
  f = zip
  g = groupby . sorted
  [y], [X] -f-> [y, X] -g-> [y, [X]] -> [gi(mean([X]), cov([X]))]
'
learn    = lambda X, y: [gi(np.mean(subX, axis=0), np.cov(subX.T)) for (y, subX) in map(partial(right, lambda x: np.array(list(map(snd, x)))), groupby(sorted(zip(y, X), key=fst), fst))]
'
  f = enumerate
  [X] -f-> [(i, X)] -> (y[i], max(gi(X)))
'
judge    = lambda X, y: [(y[fst(ix)], max(map(partial(right, partial(flip, app, snd(ix))), enumerate(G)), key=snd)) for ix in enumerate(X)]

G = learn(np.load('./data/trn_img.npy'), np.load('./data/trn_lbl.npy'))

decisions = judge(np.load('./data/dev_img.npy'), np.load('./data/dev_lbl.npy'))

error = 1 - (len([decision for decision in decisions if fst(decision) == fst(snd(decision))]) / len(decisions))

# pourcentage d'erreur sur les jugements
print(error)
