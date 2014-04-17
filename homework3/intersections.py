import matplotlib.pyplot as plt
import numpy as np
from newton import solve

x = np.linspace(-10, 10, 1000)
y1 = x * np.cos(np.pi * x)
y2 = 1 - 0.6 * (x * x)

plt.figure(1)
plt.clf()
plt.plot(x, y1, 'b-')
plt.plot(x, y2, 'r-')

def fun(x):
    f = x * np.cos(np.pi * x) - 1 + 0.6 * x**2
    fp = np.cos(np.pi * x) - x * np.sin(np.pi * x) * np.pi + 1.2 * x
    return f, fp

guesses = [-2.2, -1.6, -0.8, 1.4]
for x0 in guesses:
    print " "  # blank line
    x, iters = solve(fun, x0)
    print "solve returns x = %22.15e after %i iterations " % (x,iters)
    fx1 = x * np.cos(np.pi * x)
    fx2 = 1 - 0.6 * x**2
    print "the value of f(x) is %22.15e" % (fx1 - fx2)
    assert abs(fx1 - fx2) < 1e-14, "*** Unexpected result: x = %22.15e"  % x

    plt.plot(x, fx1, 'ko')

plt.savefig('intersections.png')
