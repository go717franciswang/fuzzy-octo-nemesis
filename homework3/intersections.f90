program intersections

    use newton, only: solve

    implicit none
    integer :: iters, itest
    real(kind=8) :: x, x0, fx
    real(kind=8) :: x0vals(4)
    real(kind=8), external :: fun, funp
    logical :: debug

    x0vals = (/-2.2, -1.6, -0.8, 1.4/)
    debug = .true.

    do itest=1,4
        x0 = x0vals(itest)
        call solve(fun, funp, x0, x, iters, debug)

        print 11, x, iters
11      format('sovler returns x = ', es22.15 , ' after', i3, 'iterations')

        fx = fun(x)
        print 12, fx
12      format('the value of f(x) is ', es22.15)

        if (abs(fx) > 1d-14) then
            print 13, x
13          format('*** Unexpected results: x = ', es22.15)
        endif
    enddo

end program intersections

real(kind=8) function fun(x)
    implicit none
    real(kind=8), intent(in) :: x
    real(kind=8) :: pi = 3.14159265358979
    fun = x * cos(pi * x) - 1 + 0.6 * x**2
end function

real(kind=8) function funp(x)
    implicit none
    real(kind=8), intent(in) :: x
    real(kind=8) :: pi = 3.14159265358979
    funp = cos(pi * x) - x * sin(pi * x) * pi + 1.2 * x
end function
