module mc_walk
    use mpi
    use problem_description, only: ax, bx, ay, by, uboundary, nx, ny, dx, dy
    implicit none

    integer :: nwalks

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
    nwalks = nwalks + 1

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

    integer :: k, iabort, ierr, num_procs, proc_num, numsent, sender
    real(kind=8) :: ub, ub_sum
    integer, dimension(MPI_STATUS_SIZE) :: status

    call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, proc_num, ierr)

    n_success = 0
    ub_sum = 0
    numsent = 0

    if (proc_num == 0) then
        do k=1,min(n_mc, num_procs-1)
            call MPI_SEND(MPI_BOTTOM, 0, MPI_INTEGER, k, 1, &
                MPI_COMM_WORLD, ierr)
            numsent = numsent + 1
        enddo

        do k=1,n_mc
            call MPI_RECV(ub, 1, MPI_DOUBLE_PRECISION, &
                MPI_ANY_SOURCE, MPI_ANY_TAG, &
                MPI_COMM_WORLD, status, ierr)
            sender = status(MPI_SOURCE)
            iabort = status(MPI_TAG)
            if (iabort == 0) then
                ub_sum = ub_sum + ub
                n_success = n_success + 1
            endif

            if (numsent < n_mc) then
                call MPI_SEND(MPI_BOTTOM, 0, MPI_INTEGER, sender, 1, &
                    MPI_COMM_WORLD, ierr)
                numsent = numsent + 1
            else
                call MPI_SEND(MPI_BOTTOM, 0, MPI_INTEGER, sender, 0, &
                    MPI_COMM_WORLD, ierr)
            endif
        enddo
    else
        do while (.true.)
            call MPI_RECV(MPI_BOTTOM, 0, MPI_INTEGER, &
                0, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
            k = status(MPI_TAG)
            if (k == 0) goto 99

            call random_walk(i0, j0, max_steps, ub, iabort)
            call MPI_SEND(ub, 1, MPI_DOUBLE_PRECISION, 0, &
                iabort, MPI_COMM_WORLD, ierr)
        enddo
    endif

    u_mc = ub_sum / n_success

99  continue
    call MPI_BARRIER(MPI_COMM_WORLD, ierr)
endsubroutine many_walks

endmodule mc_walk
