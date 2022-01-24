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
    integer(int32), dimension(:,:), allocatable :: image
    integer(int32), dimension(200) :: img_vec

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
        write(6,"(2A,I2,A,I3)") trim(my_var%name), "(", i, ")= ", n
    enddo
    
    call my_cdf%get_var("Image", my_var, stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    write(6,*) trim(my_var%name), " is number", my_var%id

    write(6,*) "Getting second record"
    allocate(image(my_var%dimsizes(1), my_var%dimsizes(2)))
    call my_var%get_record(2, image, stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    write(6,*) image(1,:)
    call my_var%get_record(2, img_vec, stat)


    call my_cdf%close(stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    
    call my_cdf%create("demo.cdf", stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)

    call my_cdf%new_var("my_variable", CDF_INT4, shape(image), my_var, stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    call my_var%add_record(image, stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)

    call my_cdf%close(stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    
    write(6,*) "OK"
end program
