
"""
Demonstration module for quadratic interpolation.
Update this docstring to describe your code.
Modified by: Francis Wang
"""


import numpy as np
import matplotlib.pyplot as plt
from numpy.linalg import solve

def quad_interp(xi,yi):
    """
    Quadratic interpolation.  Compute the coefficients of the polynomial
    interpolating the points (xi[i],yi[i]) for i = 0,1,2.
    Returns c, an array containing the coefficients of
      p(x) = c[0] + c[1]*x + c[2]*x**2.

    """

    # check inputs and print error message if not valid:

    error_message = "xi and yi should have type numpy.ndarray"
    assert (type(xi) is np.ndarray) and (type(yi) is np.ndarray), error_message

    error_message = "xi and yi should have length 3"
    assert len(xi)==3 and len(yi)==3, error_message

    # Set up linear system to interpolate through data points:

    # A = np.array([[1, xi[0], xi[0]**2], [1, xi[1], xi[1]**2], [1, xi[2], xi[2]**2]])
    A = np.vstack([np.ones(3), xi, xi**2]).T
    b = yi
    c = solve(A, b)

    return c

def plot_quad(xi, yi):
    c = quad_interp(xi, yi)
    x = np.linspace(xi.min() - 1, xi.max() + 1, 1000)
    y = c[0] + c[1]*x + c[2]*x**2

    plt.figure(1)
    plt.clf()
    plt.plot(x, y, 'b-')

    plt.plot(xi, yi, 'ro')
    # plt.ylim(y.min() - 1, y.max() + 1)
    plt.savefig('quadratic.png')

def test_quad1():
    """
    Test code, no return value or exception if test runs properly.
    """
    xi = np.array([-1.,  0.,  2.])
    yi = np.array([ 1., -1.,  7.])
    c = quad_interp(xi,yi)
    c_true = np.array([-1.,  0.,  2.])
    print "c =      ", c
    print "c_true = ", c_true
    # test that all elements have small error:
    assert np.allclose(c, c_true), \
        "Incorrect result, c = %s, Expected: c = %s" % (c,c_true)

def test_quad2():
    xi = np.array([-1.,  0.,  2.])
    yi = np.array([ 1., -1.,  7.])

    import os
    f = 'quadratic.png'
    if os.path.isfile(f):
        os.remove(f)
    plot_quad(xi,yi)
        
    assert os.path.isfile(f)

if __name__=="__main__":
    # "main program"
    # the code below is executed only if the module is executed at the command line,
    #    $ python demo2.py
    # or run from within Python, e.g. in IPython with
    #    In[ ]:  run demo2
    # not if the module is imported.
    print "Running test..."
    test_quad1()
    test_quad2()

