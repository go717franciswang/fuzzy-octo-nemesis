
program test2

    use mpi

    use quadrature, only: trapezoid
    use functions, only: f, fevals_proc, k

    implicit none
    real(kind=8) :: a,b,int_true, int_approx, h, int_approx_total, dx_sub
    real(kind=8), dimension(2) :: ab_sub

    integer :: proc_num, num_procs, ierr, n, fevals_total, j
    integer, dimension(MPI_STATUS_SIZE) :: status

    call MPI_INIT(ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, proc_num, ierr)

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
        print '("true integral: ", es22.14)', int_true
        print *, " "  ! blank line
    endif

    call MPI_BARRIER(MPI_COMM_WORLD,ierr) ! wait for process 0 to print

    ! -------------------------
    ! Master
    ! -------------------------
    if (proc_num == 0) then

        dx_sub = (b-a) / (num_procs-1)
        do j = 1,num_procs-1
            ab_sub(1) = a + (j-1)*dx_sub
            ab_sub(2) = a + j*dx_sub
            call MPI_SEND(ab_sub, 2, MPI_DOUBLE_PRECISION, j, j, &
                MPI_COMM_WORLD, ierr)
        enddo

        do j = 1,num_procs-1
            call MPI_RECV(int_approx, 1, MPI_DOUBLE_PRECISION, &
                MPI_ANY_SOURCE, MPI_ANY_TAG, &
                MPI_COMM_WORLD, status, ierr)
            int_approx_total = int_approx_total + int_approx
        enddo
    endif

    ! -------------------------
    ! Slave
    ! -------------------------
    if (proc_num /= 0) then
        call MPI_RECV(ab_sub, 2, MPI_DOUBLE_PRECISION, &
            0, proc_num, &
            MPI_COMM_WORLD, status, ierr)
        int_approx = trapezoid(f,ab_sub(1),ab_sub(2),n)

        call MPI_SEND(int_approx, 1, MPI_DOUBLE_PRECISION, 0, proc_num, &
            MPI_COMM_WORLD, ierr)

        print '("Process ", i2, " with n = ", i8, " computes int_approx = ", es22.14)', &
            proc_num, n, int_approx
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
    call MPI_FINALIZE(ierr)

end program test2
