program laplace_mc
    use problem_description, only: ax,ay,dx,dy,utrue,nx,ny
    use random_util, only: init_random_seed
    use mc_walk, only: many_walks

    implicit none
    real(kind=8) :: x0, y0, u_mc, u_mc_total, u_sum_new, u_sum_old, u_true, error
    integer :: i0, j0, maxsteps, seed, n_mc, n_success, n_total, i

    x0 = 0.9
    y0 = 0.6
    i0 = nint((x0-ax)/dx)
    j0 = nint((y0-ay)/dy)

    x0 = ax + i0*dx
    y0 = ay + j0*dy

    u_true = utrue(x0, y0)

    print 10, x0, y0, u_true
10  format ("True solution of PDE: u(",es10.3,", ",es10.3,") = ",es11.5)
    print *, "Note: with solution used in demo this is also the solution to the"
    print *, "      finite-difference equations on the same grid."

    print *, "Enter seed for random generator or <return> .... "
    read *, seed

    call init_random_seed(seed)
    maxsteps = 100*max(nx, ny)
    n_mc = 10
    call many_walks(i0, j0, maxsteps, n_mc, u_mc, n_success)

    u_mc_total = u_mc
    n_total = n_success

    do i = 1,12
        u_sum_old = u_mc_total * n_total
        call many_walks(i0, j0, maxsteps, n_mc, u_mc, n_success)
        u_sum_new = u_mc * n_success
        n_total = n_total + n_success
        u_mc_total = (u_sum_old + u_sum_new) / n_total
        error = abs((u_mc_total - u_true) / u_true)

        print '("After ",i8," random walks, u = ",es15.9,", ref. error = ",es15.6)', &
            n_total, u_mc_total, error
        n_mc = 2*n_mc
    enddo

endprogram laplace_mc
