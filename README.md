# NCIX : A modern Fortran CDF Interface

The NCIX library is a modern-ish Fortran 2003 wrapper for the CDF library.

## Usage

See the `demo.f90` file. Here is a small excerpt:

	use ncix
    implicit none
    type(CDF) :: my_cdf
    type(CDFVar) :: my_var
	call my_cdf%open("cdf_file.cdf", stat)
	if (stat .ne. NCIX_OK) call ncix_handle_error(stat)

	call my_cdf%get_var("Time", my_var, stat)
	if (stat .ne. NCIX_OK) call ncix_handle_error(stat)

	do i=1,my_var%numrecs
		call my_var%get_at_index(i, [0], n, stat)
        if (stat .ne. NCIX_OK) call ncix_handle_error(stat)
        write(6,"(2A,I2,A,I3)") trim(my_var%name), "(", i, ")= ", n
    enddo


