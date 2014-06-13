
module functions

    use omp_lib
    implicit none
    integer :: fevals(0:7)
    integer :: gevals(0:7)
    real(kind=8) :: k
    save

contains

    real(kind=8) function f(x)
        implicit none
        real(kind=8), intent(in) :: x 

        real(kind=8) :: a,b,h,y
        integer :: n,j,thread_num

        ! keep track of number of function evaluations by
        ! each thread:
        thread_num = 0   ! serial mode
        !$ thread_num = omp_get_thread_num()
        fevals(thread_num) = fevals(thread_num) + 1
        
        a = 1
        b = 4
        n = 1000
        h = (b-a)/(n-1)
        f = 0.5d0*(g(x,a) + g(x,b))
        do j=2,n-1
            y = a + (j-1)*h
            f = f + g(x,y)
            enddo
        f = h * f
        
    end function f

    real(kind=8) function g(x, y)
        implicit none
        real(kind=8), intent(in) :: x,y
        integer :: thread_num

        thread_num = 0
        !$ thread_num = omp_get_thread_num()
        gevals(thread_num) = gevals(thread_num) + 1

        g = sin(x + y)
    end function g

end module functions
