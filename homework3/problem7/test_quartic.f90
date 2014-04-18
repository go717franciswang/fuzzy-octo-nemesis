program test_quartic

    use newton, only: solve, tol
    use functions, only: f_quartic, fprime_quartic, eps

    implicit none
    real(kind=8) :: epsilons(3)
    real(kind=8) :: tolerances(3)
    real(kind=8) :: x0, x, fx, xstar
    integer :: i, j, iters
    logical :: debug

    epsilons = (/1d-4, 1d-8, 1d-12/)
    tolerances = (/1d-5, 1d-10, 1d-14/)
    debug = .false.

    print *, '    epsilon        tol    iters          x                 f(x)       x-xstar'

    do i=1,3
        do j=1,3
            eps = epsilons(i)
            tol = tolerances(j)
            x0 = 4
            call solve(f_quartic, fprime_quartic, x0, x, iters, debug)

            xstar = 1 + eps**0.25
            fx = f_quartic(x)
            print 11, eps, tol, iters, x, fx, x-xstar
11          format(2es13.3, i4, es24.15, 2es13.3)
        enddo

        print *, ''
    enddo



    
end program test_quartic
