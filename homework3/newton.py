def solve(fvals, x0, debug=False):
    precision = 1e-14
    max_iter = 100
    i = 0

    if debug:
        print "Initial guess: x = %22.15e" % (x0,)

    while i < max_iter:
        f, fp = fvals(x0)
        x1 = x0 - f/fp
        if abs(x1 - x0) < precision:
            x0 = x1
            break

        x0 = x1
        i += 1

        if debug:
            print "After %d iterations, x = %22.15e" % (i, x0)

    return x0, i

def fvals_sqrt(x):
    """
    Return f(x) and f'(x) for applying Newton to find a square root.
    """
    f = x**2 - 4.
    fp = 2.*x
    return f, fp

def test1(debug_solve=False):
    """
    Test Newton iteration for the square root with different initial
    conditions.
    """
    from numpy import sqrt
    for x0 in [1., 2., 100.]:
        print " "  # blank line
        x,iters = solve(fvals_sqrt, x0, debug=debug_solve)
        print "solve returns x = %22.15e after %i iterations " % (x,iters)
        fx,fpx = fvals_sqrt(x)
        print "the value of f(x) is %22.15e" % fx
        assert abs(x-2.) < 1e-14, "*** Unexpected result: x = %22.15e"  % x

if __name__ == "__main__":
    test1(True)

