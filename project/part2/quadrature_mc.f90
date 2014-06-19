
module quadrature_mc

contains

function quad_mc(g, a, b, ndim, npoints)

    implicit none
    real(kind=8) :: quad_mc
    real(kind=8), external :: g
    integer, intent(in) :: ndim, npoints
    real(kind=8), dimension(ndim), intent(in) :: a, b

    real(kind=8), dimension(ndim) :: diff, x
    real(kind=8), dimension(npoints) :: gs
    real(kind=4), dimension(ndim * npoints) :: r
    real(kind=8) :: v
    integer :: i

    diff = b - a
    v = product(diff)

    call random_number(r)
    do i=1,npoints
        x = r((i-1)*ndim+1 : i*ndim) * diff + a
        gs(i) = g(x, ndim)
    enddo

    quad_mc = sum(gs) * v / npoints
endfunction quad_mc

endmodule quadrature_mc

