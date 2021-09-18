    ! A(i,j) ====> i is component (x,y); j is npart (1,2,3)

subroutine leapfrog(M, H, dt, t, X, V, A)
    implicit none
    double precision, intent(in) :: dt
    double precision, dimension(2), intent(in) :: M, H
    double precision, dimension(2,2), intent(in) :: A
    double precision, dimension(2,2), intent(inout) :: X, V
    double precision, intent(inout) :: t
    double precision, dimension(2,2) :: A_pred
    integer :: i, j

    do i=1,2
        call grav(M(1), M(2), H(1), H(2), X(1:2,1), X(1:2,2), A(1:2,:))
        X(:,i) = X(:,i) + V(:,i)*dt + 0.5*A(:,i)*dt**2
        call grav(M(1), M(2), H(1), H(2), X(1:2,1), X(1:2,2), A_pred(1:2,:))
        V(:,i) = V(:,i) + 0.5*(A(:,i) + A_pred(:,i))*dt

    enddo
    
    t = t + dt
end subroutine leapfrog