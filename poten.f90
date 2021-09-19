subroutine accel(m1, m2, m3, h1, h2, h3, r1, r2, r3, a)
    implicit none
    double precision, intent(in) :: m1, m2, m3, h1, h2, h3
    double precision, dimension(2), intent(in) :: r1, r2, r3
    double precision, dimension(2,3), intent(out) :: a
    double precision, dimension(2) :: f1, f2, f3, a1, a2, a3
    double precision, dimension(2,3) :: th
    double precision, dimension(3) :: dist
    integer :: i

    f1(:) = 0d0
    f2(:) = 0d0
    f3(:) = 0d0

    call mag_dir(r1,r2,dist(1),th(1:2,1))
    call mag_dir(r2,r3,dist(2),th(1:2,2))
    call mag_dir(r1,r3,dist(3),th(1:2,3))

    call grav(dist(1),th(1:2,1),m1,m2,h1,h2,f1,f2)
    call grav(dist(2),th(1:2,2),m2,m3,h2,h3,f2,f3)
    call grav(dist(3),th(1:2,3),m1,m3,h1,h3,f1,f3)

    a1(:) = f1(:)/m1
    a2(:) = f2(:)/m2
    a3(:) = f3(:)/m3

    a(1:2,2) = a1
    a(1:2,2) = a2
    a(1:2,3) = a3

end subroutine accel


!Here the angle "th" defines the n-force calculated:
subroutine grav(rr,th,m_1,m_2,h_1,h_2,f1,f2)
    implicit none
    double precision, intent(in) :: rr,m_1,m_2,h_1,h_2
    double precision, dimension(2), intent(in) :: th
    double precision, dimension(2), intent(inout) :: f1,f2
    double precision, dimension(2) :: f
    integer :: i
    do i=1,2 
        if (rr < (h_1 + h_2)) then
            f(i) = th(i)*m_1*m_2/(h_1 + h_2)**2 
        else
            f(i) = th(i)*m_1*m_2/rr**2
        end if
    f1(i) = f1(i) + f(i)                                    
    f2(i) = f2(i) - f(i)                                    !Garantees conservation of linear momentum (f12=-f21)
    enddo
    
end subroutine grav

subroutine mag_dir(r_1,r_2,rr,th)
    implicit none
    double precision, dimension(2), intent(in) :: r_1, r_2
    double precision, intent(out) :: rr
    double precision, dimension(2), intent(out) :: th
    
    rr = sqrt((r_1(1) - r_2(1))**2 + (r_1(2) - r_2(2))**2)
    th(1) = (r_2(1) - r_1(1))/rr                             !x-component of r vector
    th(2) = (r_2(2) - r_1(2))/rr                             !y-component of r vector

end subroutine mag_dir

