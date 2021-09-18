subroutine grav(m1, m2, h1, h2, r1, r2, a)
    implicit none
    double precision, intent(in) :: m1, m2, h1, h2
    double precision, dimension(2), intent(in) :: r1, r2
    double precision, dimension(2,2), intent(out) :: a
    double precision, dimension(2) :: th
    double precision :: r
    integer :: i

    call magnitude(r1,r2,r)
    th(1) = (r2(1) - r1(1))/r                             !x-component of r vector
    th(2) = (r2(2) - r1(2))/r                             !y-component of r vector
    if (r<(h1+h2)) then
        do i=1,2                                            !x,y coordinates
            a(i,1) = th(i)*m2/(h1+h2)**2
        enddo
    else 
        do i=1,2                                            !x,y coordinates
            a(i,1) = th(i)*m2/r**2
        enddo
    end if
    a(:,2) = -a(:,1)*(m1/m2)                                        !Garantees conservation of linear momentum (f12=-f21)

end subroutine grav



subroutine magnitude(r1,r2,r)
    implicit none
    double precision, dimension(2), intent(in) :: r1, r2
    double precision, intent(out) :: r
    
    r = sqrt((r1(1) - r2(1))**2 + (r1(2) - r2(2))**2)

end subroutine magnitude