!testing
! Used for testing part of the whole code
! A(i,j) ====> i is component (x,y); j is npart (1,2,3)
program test

    implicit none
    double precision :: g, t_units, r_units, vel_units, m_units, pi
    double precision :: ener_units, spec_ener_units, ang_mom_units
    double precision :: spec_ang_mom_units
    double precision :: m1, m2, m3, dt, t, h1, h2, h3
    double precision :: tf, ndump, dtprint
    double precision :: ekin, epot, etot0
    double precision, dimension(3) ::  M, H
    double precision, dimension(2,3) :: X,V,A
    double precision, dimension(2) ::  r1, r2, r3, v1, v2, v3
    
    integer :: i
    call units(g, t_units, r_units, vel_units, m_units, pi, ener_units, spec_ener_units, &
                    ang_mom_units, spec_ang_mom_units)

    
    
    
    tf = 407.14
    dt = 0.01

    ndump = 0.0
    dtprint = 0.5

    m1 = 3.33d5
    m2 = 1d0
    m3 = 317.8


    h1 = 0.001
    h2 = 0.001
    h3 = 0.001


    r1(1) = 0.0
    r1(2) = 0.0

    r2(1) = 218.623
    r2(2) = 0
    
    r3(1) = 1119.1
    r3(2) = 0


    v1(1) = 0
    v1(2) = 0

    v2(1) = 0
    v2(2) = 38.7
    
    v3(1) = 0
    v3(2) = 17.25


    X(1,1) = r1(1)
    X(2,1) = r1(2)
    X(1,2) = r2(1)
    X(2,2) = r2(2)
    X(1,3) = r3(1)
    X(2,3) = r3(2)


    V(1,1) = v1(1)
    V(2,1) = v1(2)
    V(1,2) = v2(1)
    V(2,2) = v2(2)
    V(1,3) = v3(1)
    V(2,3) = v3(2)

    M(1) = m1
    M(2) = m2
    M(3) = m3

    H(1) = h1
    H(2) = h2
    H(3) = h3

    call accel(m1, m2, m3, h1, h2, h3, r1, r2, r3, A)
    call pot(m1, m2, m3, h1, h2, h3, X(1:2,1), X(1:2,2), X(1:2,3), epot)
    call kin(M, V, ekin)
    etot0 = epot + ekin
    open(unit=665, file='sun.ev', status = 'replace')
    open(unit=666, file='earth.ev', status = 'replace')
    open(unit=667, file='jupiter.ev', status = 'replace')
    open(unit=668, file='energy.ev', status = 'replace')
    
    do i=665,667
        write(i,*) '              t   ', '                 x   ', '                     y ',&
                   '                     vx   ', '                    vy '
    enddo
    
    write(668,*) '              t   ','             epot     ', '            ekin   ', '                 etot ',&
                   '                       energy_error   '

    
    do while (t<tf)
        if (t>=ndump*dtprint) then
            do i=1,3
                write(i+664,*) t, X(:,i), V(:,i)
            enddo
            write(668,*) t,epot, ekin, ekin+epot, abs((ekin+epot - etot0)/etot0)
            call pot(m1, m2, m3, h1, h2, h3, X(1:2,1), X(1:2,2), X(1:2,3), epot)
            call kin(M, V, ekin)
            print*,t
            ndump = ndump + 1
        end if
        call leapfrog(M, H, dt, t, X, V, A)
    end do


end program test



