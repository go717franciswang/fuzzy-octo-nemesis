
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

    error_message = "xi and yi should have length 3"
    assert len(xi)==3 and len(yi)==3, error_message

    # Set up linear system to interpolate through data points:

    return poly_interp(xi, yi)

def cubic_interp(xi, yi):
    error_message = "xi and yi should have length 4"
    assert len(xi)==4 and len(yi)==4, error_message

    return poly_interp(xi, yi)

def poly_interp(xi, yi):
    error_message = "xi and yi should have type numpy.ndarray"
    assert (type(xi) is np.ndarray) and (type(yi) is np.ndarray), error_message

    assert len(xi) > 0 and len(xi) == len(yi), "invalid length"

    n = len(xi)
    A = np.vstack([np.ones(n)*xi**p for p in range(n)]).T
    b = yi
    c = solve(A, b)

    return c

def plot_quad(xi, yi):
    plot_poly(xi, yi, 'quadratic.png')

def plot_cubic(xi, yi):
    plot_poly(xi, yi, 'cubic.png')

def plot_poly(xi, yi, f='polynomial.png'):
    c = poly_interp(xi, yi)
    x = np.linspace(xi.min() - 1, xi.max() + 1, 1000)
    n = len(xi)
    y = c[n-1]
    for j in range(n-1, 0, -1):
        y = y*x + c[j-1]

    plt.figure(1)
    plt.clf()
    plt.plot(x, y, 'b-')

    plt.plot(xi, yi, 'ro')
    plt.savefig(f)

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
    xi = np.array([-1., 1.,  2.])
    yi = np.array([ 1., 1.,  7.])
    assert_png_generated(lambda : plot_quad(xi, yi), 'quadratic.png')

def assert_png_generated(func, filename):
    import os
    if os.path.isfile(filename):
        os.remove(filename)
    func()
    assert os.path.isfile(filename)

def test_cubic1():
    xi = np.array([1, 2, 3, 4])
    yi = np.array([1, 2, 3, 4])
    c = cubic_interp(xi, yi)
    c_true = [0, 1, 0, 0]
    assert np.allclose(c, c_true)

def test_cubic2():
    xi = np.array([1, 2, 3, 4])
    yi = np.array([1, 2, 3, 4])
    assert_png_generated(lambda : plot_cubic(xi, yi), 'cubic.png')

def test_poly1():
    xi = np.array(range(5))
    yi = np.array(range(5))
    c = poly_interp(xi, yi)
    c_true = [0, 1, 0, 0, 0]
    assert np.allclose(c, c_true)

    xi = np.array(range(6))
    yi = np.array(range(6))
    c = poly_interp(xi, yi)
    c_true = [0, 1, 0, 0, 0, 0]
    assert np.allclose(c, c_true)

def test_poly2():
    xi = np.array(range(6))
    yi = np.array(range(6))
    assert_png_generated(lambda : plot_poly(xi, yi), 'polynomial.png')

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
    test_cubic1()
    test_cubic2()
    test_poly1()
    test_poly2()

