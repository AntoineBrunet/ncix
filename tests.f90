program TEST
    use ncix
    implicit none
    type(CDF) :: my_cdf
    type(CDFVar) :: my_var
    integer :: n,i,j
    integer :: stat
    integer(int32), dimension(:,:), allocatable :: image
    integer(int32), dimension(200) :: img_vec
    
    type(CDFEpoch) :: cepoch
    type(DetailEpoch) :: depoch
    
    real(real32) :: rval
    real(real64) :: dval

    call my_cdf%open("../data/example1.cdf", stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)

    call my_cdf%get_nb_var(n, stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    if (n .ne. 3) call fail("Wrong number of variables")

    call my_cdf%get_var("Time", my_var, stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    if (my_var%id .ne. 1) call fail("Wrong variable id")
    if (my_var%numrecs .ne. 2) call fail("Wrong number of records")

    do i=1,my_var%numrecs
        call my_var%get_at_index(i,[0], n, stat)
        if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    enddo
    
    call my_cdf%get_var("Image", my_var, stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    if (my_var%id .ne. 3) call fail("Wrong variable id")
    if (my_var%numrecs .ne. 3) call fail("Wrong number of records")
    if (my_var%dimsizes(1) .ne. 10) call fail("Wrong 1 dimension size")
    if (my_var%dimsizes(2) .ne. 20) call fail("Wrong 2 dimension size")

    allocate(image(my_var%dimsizes(1), my_var%dimsizes(2)))
    call my_var%get_record(2, image, stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)

    do I=1,my_var%dimsizes(1)
        do J=1,my_var%dimsizes(2)
            n = 189 + I + J*10
            if (image(I,J) .ne. N) call fail("Error loading matrix")
        enddo
    enddo
    deallocate(image)

    call my_var%get_record(2, img_vec, stat)
    do I=1,200
        n = 199+I
        if (img_vec(I) .ne. N) call fail("Error loading vector")
    enddo

    call my_cdf%close(stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)

    ! removing testfile.cdf
    open(unit=1234, iostat=stat, file='testfile.cdf', status='old')
    if (stat == 0) close(1234, status='delete')

    call my_cdf%create('testfile.cdf', stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    call my_cdf%new_var("real", CDF_REAL4, [3,4], my_var, stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    if (my_var%numrecs .ne. 0) call fail("New var not empty")
    if (my_var%data_type .ne. CDF_REAL4) call fail("New var wrong type")
    if (my_var%numdims .ne. 2) call fail("New var wrong number of dims")
    if (my_var%dimsizes(1) .ne. 3) call fail("New var wrong dim 1")
    if (my_var%dimsizes(2) .ne. 4) call fail("New var wrong dim 2")

    call my_var%add_record([1.0, 2.0, 3.0, &
            4.0, 5.0, 6.0, &
            7.0, 8.0, 9.0, &
           10.0,11.0, 12.0], stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    if (my_var%numrecs .ne. 1) call fail("Var should have 1 record")

    call my_cdf%new_var("double", CDF_REAL8, [2,3], my_var, stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    if (my_var%numrecs .ne. 0) call fail("New var not empty")
    if (my_var%data_type .ne. CDF_REAL8) call fail("New var wrong type")
    if (my_var%numdims .ne. 2) call fail("New var wrong number of dims")
    if (my_var%dimsizes(1) .ne. 2) call fail("New var wrong dim 1")
    if (my_var%dimsizes(2) .ne. 3) call fail("New var wrong dim 2")

    call my_var%add_record([[11.d0, 12.d0],[21.d0,22.d0], [31.d0,32.d0]], stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    if (my_var%numrecs .ne. 1) call fail("Var should have 1 record")
    call my_var%add_record([111.d0, 112.d0,121.d0,122.d0, 131.d0,132.d0], stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    if (my_var%numrecs .ne. 2) call fail("Var should have 2 record")
    
    call my_cdf%new_var("epoch", CDF_EPOCH, NCIX_NODIM, my_var, stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    depoch%year = 2022
    depoch%month = 1
    depoch%day = 1
    call cepoch%from_details(depoch)
    call my_var%put_record(1, cepoch, stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)

    call my_cdf%close(stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)

    call my_cdf%open("testfile.cdf", stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    call my_cdf%get_var("real", my_var, stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    if (my_var%numrecs .ne. 1) call fail("Var should have 1 record")
    call my_var%get_at_index(1,[2,3],rval, stat)  
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    if (rval .ne. 8.0) call fail("real[2,3] should be 8.0")

    call my_cdf%get_var("double", my_var, stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    if (my_var%numrecs .ne. 2) call fail("Var should have 2 record")
    call my_var%get_at_index(1,[1,2],dval, stat)  
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    if (dval .ne. 21.0d0) call fail("double[1,2] should be 21.0d0")
    call my_var%get_at_index(2,[1,2],dval, stat)  
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    if (dval .ne. 121.0d0) call fail("double[1,2] should be 121.0d0")

    call my_cdf%close(stat)
    if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
    write(6,*) "OK"
contains
    subroutine fail(message) 
            character(len=*), intent(in) :: message
            write(*,*) "Fail with: ", message
            call my_cdf%close(stat)
            stop
    end subroutine
end program
