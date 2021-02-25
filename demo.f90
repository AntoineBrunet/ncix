program DEMO
    use ncix
    implicit none
    type(CDF) :: my_cdf
    type(CDFVar) :: my_var
    type(CDFEpoch) :: epoch
    type(CDFEpoch16) :: epoch16
    type(DetailEpoch) :: d_epoch
    integer :: n,i
    integer :: stat
    integer :: indices(3)

    call my_cdf%open("../data/example1.cdf", stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)

    call my_cdf%get_nb_var(n, stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    write(6,*) n,"variables"

    do i=1,n
        call my_cdf%get_var(i, my_var, stat)
        write(6,*) "-",trim(my_var%name), ' : ', my_var%numrecs, my_var%dimsizes(:my_var%numdims)
        write(6,*) my_var%dimvariances(:my_var%numdims)
    enddo

    call my_cdf%get_var("Time", my_var, stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    write(6,*) trim(my_var%name), " is number", my_var%id

    do i=1,my_var%numrecs
        call my_var%get_at_index(i,[0], n, stat)
        if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
        write(6,*) trim(my_var%name), i, "  has value", n
    enddo
    

    call my_cdf%close(stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    write(6,*) "OK"
end program
