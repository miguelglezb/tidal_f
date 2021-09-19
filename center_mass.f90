subroutine CoM(M, X, X_CoM)
    implicit none
    double precision, dimension(3), intent(in) :: M
    double precision, dimension(2,3), intent(in) :: X 
    double precision, dimension(2), intent(out) :: X_CoM 
    double precision :: M_tot
    integer :: i

    X_CoM(:) = 0.0
    !V_CoM(:,:) = 0.0
    M_tot = 0.0

    do i=1,3
        M_tot = M_tot + M(i)
    enddo
    do i=1,3
        X_CoM(1) = X_CoM(1) + M(i)*X(1,i)
        X_CoM(2) = X_CoM(2) + M(i)*X(2,i)
    enddo
    X_CoM(:) = X_CoM(:)/M_tot
end subroutine 

