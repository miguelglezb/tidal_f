!testing
! Used for testing part of the whole code
! A(i,j) ====> i is component (x,y); j is npart (1,2,3)
program test

    implicit none
    double precision :: g, t_units, r_units, vel_units, m_units, pi
    double precision :: ener_units, spec_ener_units, ang_mom_units
    double precision :: spec_ang_mom_units
    double precision :: m1, m2, dt, t, h1, h2
    double precision, dimension(2) :: r1, r2, v1, v2, M, H
    double precision, dimension(2,2) :: X,V,A
    integer :: i
    call units(g, t_units, r_units, vel_units, m_units, pi, ener_units, spec_ener_units, &
                    ang_mom_units, spec_ang_mom_units)

    m1 = 3.33d5
    m2 = 1d0
    h1 = 0.001
    h2 = 0.001
    dt = 0.1
    
    t = 0d0
    r1(1) = 0d0
    r1(2) = 0d0
    r2(1) = 218.623
    r2(2) = 0

    v1(1) = 0
    v1(2) = 0
    v2(1) = 0
    v2(2) = 38.7
    
    X(1,1) = r1(1)
    X(2,1) = r1(2)
    X(1,2) = r2(1)
    X(2,2) = r2(2)

    V(1,1) = v1(1)
    V(2,1) = v1(2)
    V(1,2) = v2(1)
    V(2,2) = v2(2)

    M(1) = m1
    M(2) = m2

    H(1) = h1
    H(2) = h2

    call grav(m1, m2, h1, h2, r1, r2, A)
    open(unit=667, file='kk.ev', status = 'old')
    print*,r_units*r1,r_units*r2
    write(667,*) '                 x   ', '                y '
    do i=1,36500
        write(667,*) X(:,2)
        call leapfrog(M, H, dt, t, X, V, A)
    enddo
    print*,t
end program test



