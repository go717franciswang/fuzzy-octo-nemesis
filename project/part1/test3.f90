
program test3

    use mpi

    use quadrature, only: trapezoid
    use functions, only: f, fevals_proc, k

    implicit none
    real(kind=8) :: a,b,int_true, int_approx, h, int_approx_total, dx_sub
    real(kind=8), dimension(2) :: ab_sub

    integer :: proc_num, num_procs, ierr, n, fevals_total, j, nsub, sender, numsent
    integer, dimension(MPI_STATUS_SIZE) :: status

    call MPI_INIT(ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, proc_num, ierr)

    if (num_procs < 2) then
        print *, "***  Error: need to use at least two processes"
        goto 99
    endif

    ! All processes set these values so we don't have to broadcast:
    k = 1.d3   ! functions module variable 
    a = 0.d0
    b = 2.d0
    int_true = (b-a) + (b**4 - a**4) / 4.d0 - (1.d0/k) * (cos(k*b) - cos(k*a))
    n = 1000

    ! Each process keeps track of number of fevals:
    fevals_proc = 0

    if (proc_num==0) then
        print '("Using ",i3," processes")', num_procs

        print *, "How many subintervals? "
        read *, nsub

        print '("true integral: ", es22.14)', int_true
        print *, " "  ! blank line
    endif

    call MPI_BCAST(nsub, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)

    ! -------------------------
    ! Master
    ! -------------------------
    if (proc_num == 0) then
        numsent = 0

        dx_sub = (b-a) / nsub
        do j = 1,min(nsub, num_procs-1)
            ab_sub(1) = a + (j-1)*dx_sub
            ab_sub(2) = a + j*dx_sub
            call MPI_SEND(ab_sub, 2, MPI_DOUBLE_PRECISION, j, j, &
                MPI_COMM_WORLD, ierr)
            numsent = numsent + 1
        enddo

        do j = 1,nsub
            call MPI_RECV(int_approx, 1, MPI_DOUBLE_PRECISION, &
                MPI_ANY_SOURCE, MPI_ANY_TAG, &
                MPI_COMM_WORLD, status, ierr)
            int_approx_total = int_approx_total + int_approx
            sender = status(MPI_SOURCE)

            if (numsent < nsub) then
                ab_sub(1) = a + numsent * dx_sub
                ab_sub(2) = a + (numsent+1) * dx_sub
                numsent = numsent + 1
                call MPI_SEND(ab_sub, 2, MPI_DOUBLE_PRECISION, &
                    sender, numsent, MPI_COMM_WORLD, ierr)
            else
                call MPI_SEND(MPI_BOTTOM, 0, MPI_DOUBLE_PRECISION, &
                    sender, 0, MPI_COMM_WORLD, ierr)
            endif
        enddo
    endif

    ! -------------------------
    ! Slave
    ! -------------------------
    if (proc_num /= 0) then
        do while (proc_num <= nsub)
            call MPI_RECV(ab_sub, 2, MPI_DOUBLE_PRECISION, &
                0, MPI_ANY_TAG, &
                MPI_COMM_WORLD, status, ierr)
            j = status(MPI_TAG)
            if (j == 0) then ! 0 is the exit status
                exit
            endif

            int_approx = trapezoid(f,ab_sub(1),ab_sub(2),n)

            call MPI_SEND(int_approx, 1, MPI_DOUBLE_PRECISION, 0, proc_num, &
                MPI_COMM_WORLD, ierr)
        enddo
    endif

    call MPI_BARRIER(MPI_COMM_WORLD,ierr)
    print '("fevals by Process ",i2,": ",i13)',  proc_num, fevals_proc
    call MPI_REDUCE(fevals_proc, fevals_total, 1, MPI_INTEGER, MPI_SUM, 0, &
                    MPI_COMM_WORLD, ierr)

    if (proc_num == 0) then
        print '("Total number of fevals: ",i10)', fevals_total
        print '("int_approx = ",es22.14)', int_approx_total
    endif

    call MPI_BARRIER(MPI_COMM_WORLD,ierr)

99 continue ! used to exit early
    call MPI_FINALIZE(ierr)

end program test3
