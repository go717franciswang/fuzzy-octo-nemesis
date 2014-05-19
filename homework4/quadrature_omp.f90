module quadrature_omp
use omp_lib

contains

real(kind=8) function trapezoid(f, a, b, n)
    implicit none

    integer :: thread_num

    real(kind=8), external :: f
    real(kind=8), intent(in) :: a, b
    integer, intent(in) :: n

    real(kind=8) :: y0, y1, h, x
    integer :: i
    real(kind=8), dimension(n) :: fj

    h = (b-a) / (n-1)
    x = a
    do i=1,n
        fj(i) = f(x)
        x = x + h
    end do

    trapezoid = 0.
    !$omp parallel do
    do i=1,n
        trapezoid = trapezoid + fj(i)
    end do
    !$omp end parallel do

    trapezoid = trapezoid*h - 0.5*h*(fj(1) + fj(size(fj)))
end function trapezoid

subroutine error_table(f, a, b, nvals, int_true)
    implicit none

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

end module quadrature_omp
