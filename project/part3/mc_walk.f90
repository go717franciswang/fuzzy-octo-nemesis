module mc_walk
    use problem_description, only: ax, bx, ay, by, uboundary, nx, ny, dx, dy
    implicit none

contains

subroutine random_walk(i0, j0, max_steps, ub, iabort)
    integer, intent(in) :: i0, j0, max_steps
    real(kind=8), intent(out) :: ub
    integer, intent(out) :: iabort

    real(kind=4) :: r(max_steps)
    real(kind=8) :: xb, yb
    integer :: i, j, k

    i = i0
    j = j0
    iabort = 0

    call random_number(r)
    do k = 1,max_steps
        if (r(k) < 0.25) then
            i = i-1
        elseif (r(k) < 0.5) then
            i = i+1
        elseif (r(k) < 0.75) then
            j = j-1
        else
            j = j+1
        endif

        if (i*j*(nx+1-i)*(ny+1-j) == 0) then
            xb = ax + i*dx
            yb = ay + j*dy
            ub = uboundary(xb, yb)
            return
        endif

        if (k == max_steps) then
            iabort = 1
        endif
    enddo
endsubroutine

subroutine many_walks(i0, j0, max_steps, n_mc, u_mc, n_success)
    integer, intent(in) :: i0, j0, max_steps, n_mc
    real(kind=8), intent(out) :: u_mc
    integer, intent(out) :: n_success

    integer :: k, i, j, iabort
    real(kind=8) :: ub, ub_sum

    n_success = 0
    ub_sum = 0

    do k=1,n_mc
        call random_walk(i0, j0, max_steps, ub, iabort)
        if (iabort == 0) then
            ub_sum = ub_sum + ub
            n_success = n_success + 1
        endif
    enddo

    u_mc = ub_sum / n_success
endsubroutine many_walks

endmodule mc_walk
