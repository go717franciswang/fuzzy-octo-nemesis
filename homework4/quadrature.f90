module quadrature

contains

real(kind=8) function trapezoid(f, a, b, n)
    implicit none

    real(kind=8), external :: f
    real(kind=8), intent(in) :: a, b
    integer, intent(in) :: n

    real(kind=8) :: y0, y1, h, x
    integer :: i

    h = (b-a) / (n-1)
    trapezoid = 0.
    x = a
    y0 = f(x)
    do i=1,n
        x = x + h
        y1 = f(x)
        trapezoid = trapezoid + (y1+y0)*h/2
        y0 = y1
    end do
end function trapezoid

subroutine error_table(f, a, b, nvals, int_true)
    real(kind=8), external :: f
    real(kind=8), intent(in) :: a, b, int_true
    integer, dimension(:), intent(in) :: nvals

    integer :: i,n
    real(kind=8) :: int_trap, error, last_error, ratio

    print *, "    n         trapezoid            error       ratio"
    do i=1,size(nvals)
        n = nvals(i)
        int_trap = trapezoid(f, a, b, nvals(i))
        error = abs(int_trap - int_true)
        ratio = last_error / error
        last_error = error

        print 11, n, int_trap, error, ratio
11      format(i8, es22.14, es13.3, es13.3)
    end do
end subroutine error_table

end module quadrature
