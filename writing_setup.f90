subroutine w_setup(filename, headers, ncols, data)
    implicit none
    character(len=17), allocatable :: columns(:)
    character(len=17) :: filename
    integer :: ncols


    allocate(columns(ncols))