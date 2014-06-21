module mc_walk
    use problem_description, only: ax, bx, ay, by, uboundary, nx, ny, dx, dy

contains

subroutine random_walk(i0, j0, max_steps, ub, iabort)
    implicit none

    integer, intent(in) :: i0, j0, max_steps
    real(kind=8), intent(out) :: ub
    integer, integer(out) :: iabort

    real(kind=4) :: r(max_steps)
    integer :: i, j

    i = i0
    j = j0
    iabort = 0

    call random_number(r)
    do i = 1,max_steps
        if r(i) < 0.25 then
            i = i-1
        elseif r(i) < 0.5 then
            i = i+1
        elseif r(i) < 0.75 then
            j = j-1
        else
            j = j+1
        endif

        if i*j*(nx+1-i)*(ny+1-j) == 0 then
            xb = ax + i*dx
            yb = ay + j*dy
            ub = uboundary(xb, yb)
            return
        endif

        if i == max_steps then
            iabort = 1
        endif
    enddo
endsubroutine

endmodule mc_walk
