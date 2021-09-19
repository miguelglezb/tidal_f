subroutine pot(m1, m2, m3, h1, h2, h3, r1, r2, r3, epot)
    implicit none
    double precision, intent(in) :: m1, m2, m3, h1, h2, h3
    double precision, dimension(2), intent(in) :: r1, r2, r3
    double precision, intent(out) :: epot
    double precision, dimension(3) :: dist
    double precision, dimension(2,3) :: th
    integer :: i

    call mag_dir(r1,r2,dist(1),th(1:2,1))
    call mag_dir(r2,r3,dist(2),th(1:2,2))
    call mag_dir(r1,r3,dist(3),th(1:2,3))

    if (dist(1) < (h1 + h2)) then
        dist(1) = h1 + h2 
    end if

    if (dist(2) < (h2 + h3)) then
        dist(2) = h2 + h3 
    end if

    if (dist(3) < (h1 + h3)) then
        dist(3) = h1 + h3 
    end if
    
    epot = -(m1*m2)/dist(1) - (m2*m3)/dist(2) - (m1*m3)/dist(3)

end subroutine pot


subroutine kin(M, V, ekin)
    implicit none
    double precision, dimension(3), intent(in) :: M
    double precision, dimension(2,3), intent(in) :: V
    double precision, intent(out) :: ekin
    integer :: i


    ekin = 0.0
    do i=1,3
        ekin = ekin + 0.5*M(i)*(V(1,i)**2+V(2,i)**2)
    enddo
end subroutine kin


