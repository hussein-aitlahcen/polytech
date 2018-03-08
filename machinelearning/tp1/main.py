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

data = np.array([[[0, 0], [0, 1]], [[1, 0], [1, 1], [4, 0], [4, 1]]])

def perceptron(w, v, i, j, k):
    if j == len(w):
        return (v, k)
    wi = w[i]
    wv = np.vdot(wi, v)
    nextI = (i + 1) % len(w)
    nextK = k + 1
    return perceptron(w, v, nextI, j + 1, nextK) if wv > 0 else perceptron(w, np.add(wi, v), nextI, 0, nextK)

def add_signum(wx, sign):
    return np.apply_along_axis(lambda v: np.concatenate(([sign], v * sign), 0), 1, wx)

def main():
    w1 = data[0]
    w2 = data[1]
    w = np.concatenate((add_signum(w1, 1), add_signum(w2, -1)), 0)
    a = np.array([1, 3, 6])
    (v, k) = perceptron(w, a, 0, 0, 0)
    print("frontier: " + str(v))
    print("iterations: " + str(k))

main()
