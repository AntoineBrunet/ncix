module ncix
    use iso_fortran_env, int8_t => int8
    use iso_c_binding
    use cdf_mod, int8 => cdf_int8
    use ncix_epoch
#ifdef DATETIME_FORTRAN
    use datetime_module, only:datetime
#endif
    implicit none
    public
    integer, parameter :: NCIX_OK = CDF_OK

    type :: CDF
        integer :: id
    contains
        procedure :: open => ncix_open
        procedure :: close => ncix_close
        procedure, pass :: ncix_get_var_by_id
        procedure, pass :: ncix_get_var_by_name
        generic :: get_var =>  ncix_get_var_by_id, ncix_get_var_by_name
        procedure :: get_nb_var => ncix_get_nb_var
    end type CDF

    type :: CDFVar
        type(CDF) :: cdf
        integer :: id
        integer :: data_type
        integer :: numrecs
        integer :: numdims
        integer :: dimsizes(CDF_MAX_DIMS)
        integer :: dimvariances(CDF_MAX_DIMS)
        character(len=CDF_VAR_NAME_LEN256) :: name
    contains
        procedure :: init => ncix_var_init 
        procedure :: get_record_buffer_size => ncix_var_get_record_buffer_size
        procedure, pass :: ncix_var_get_byte_at_index
        procedure, pass :: ncix_var_get_int2_at_index
        procedure, pass :: ncix_var_get_int4_at_index
        procedure, pass :: ncix_var_get_float_at_index
        procedure, pass :: ncix_var_get_double_at_index
        procedure, pass :: ncix_var_get_epoch_at_index
        procedure, pass :: ncix_var_get_epoch16_at_index
        procedure, pass :: ncix_var_get_detailepoch_at_index
#ifdef DATETIME_FORTRAN
        procedure, pass :: ncix_var_get_datetime_at_index
#endif

        generic :: get_at_index => ncix_var_get_byte_at_index, &
                              ncix_var_get_int2_at_index, &
                              ncix_var_get_int4_at_index, &
                              ncix_var_get_epoch_at_index, &
                              ncix_var_get_epoch16_at_index, &
                              ncix_var_get_detailepoch_at_index, &
#ifdef DATETIME_FORTRAN
                              ncix_var_get_datetime_at_index, &
#endif
                              ncix_var_get_float_at_index, &
                              ncix_var_get_double_at_index

        procedure, pass :: ncix_var_get_float_record
        procedure, pass :: ncix_var_get_float_record_vec
        procedure, pass :: ncix_var_get_float_record_mat
        procedure, pass :: ncix_var_get_float_record_ter
        procedure, pass :: ncix_var_get_float_record_qad
        procedure, pass :: ncix_var_get_double_record
        procedure, pass :: ncix_var_get_double_record_vec
        procedure, pass :: ncix_var_get_double_record_mat
        procedure, pass :: ncix_var_get_double_record_ter
        procedure, pass :: ncix_var_get_double_record_qad
        procedure, pass :: ncix_var_get_int2_record
        procedure, pass :: ncix_var_get_int2_record_vec
        procedure, pass :: ncix_var_get_int2_record_mat
        procedure, pass :: ncix_var_get_int2_record_ter
        procedure, pass :: ncix_var_get_int2_record_qad
        procedure, pass :: ncix_var_get_int4_record
        procedure, pass :: ncix_var_get_int4_record_vec
        procedure, pass :: ncix_var_get_int4_record_mat
        procedure, pass :: ncix_var_get_int4_record_ter
        procedure, pass :: ncix_var_get_int4_record_qad
        generic :: get_record => ncix_var_get_float_record, &
                            ncix_var_get_float_record_vec, &
                            ncix_var_get_float_record_mat, &
                            ncix_var_get_float_record_ter, &
                            ncix_var_get_float_record_qad, &
                            ncix_var_get_double_record, &
                            ncix_var_get_double_record_vec, &
                            ncix_var_get_double_record_mat, &
                            ncix_var_get_double_record_ter, &
                            ncix_var_get_double_record_qad, &
                            ncix_var_get_int2_record, &
                            ncix_var_get_int2_record_vec, &
                            ncix_var_get_int2_record_mat, &
                            ncix_var_get_int2_record_ter, &
                            ncix_var_get_int2_record_qad, &
                            ncix_var_get_int4_record, &
                            ncix_var_get_int4_record_vec, &
                            ncix_var_get_int4_record_mat, &
                            ncix_var_get_int4_record_ter, &
                            ncix_var_get_int4_record_qad
    end type
    
contains

    subroutine ncix_handle_error(status)
        integer, intent(in) :: status
        character(len=CDF_STATUSTEXT_LEN) ::  message
        integer :: stat
        if (status .eq. CDF_OK) return
        call CDF_error(status, message, stat)
        if (status .lt. CDF_WARN) then
            write(0,*)  trim(message)
            stop
        else
            write(0,*)  trim(message)
        endif
    end subroutine

    subroutine ncix_open(this, filename, status)
        class(CDF), intent(inout) :: this
        character(LEN=*) , intent(in) :: filename
        integer, intent(out) :: status
        call CDF_open(filename, this%id, status)
        if (status .ne. CDF_OK) return
        call CDF_set_zmode(this%id, 2, status)
        if (status .ne. CDF_OK) return
    end subroutine

    subroutine ncix_close(this, status)
        class(CDF), intent(inout) :: this
        integer, intent(out) :: status
        call CDF_close(this%id, status)
        if (status .ne. CDF_OK) return
    end subroutine

    include "ncix-get.h"

    subroutine ncix_var_init(this, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(out) :: status
        call CDF_get_zvar_datatype(this%cdf%id, this%id, this%data_type, status)
        if (status .ne. CDF_OK) return
        call CDF_get_zvar_maxwrittenrecnum(this%cdf%id, this%id, this%numrecs, status)
        if (status .ne. CDF_OK) return
        call CDF_get_zvar_numdims(this%cdf%id, this%id, this%numdims, status)
        if (status .ne. CDF_OK) return
        if (this%numdims .gt. 0) then
            call CDF_get_zvar_dimsizes(this%cdf%id, this%id, this%dimsizes, status)
            if (status .ne. CDF_OK) return
            call CDF_get_zvar_dimvariances(this%cdf%id, this%id, this%dimvariances, status)
            if (status .ne. CDF_OK) return
        endif
    end subroutine

    function ncix_var_get_record_buffer_size(this) result(sze)
        class(CDFVar), intent(in) :: this
        integer :: sze
        integer :: I
        sze = 1
        do I=1,this%numdims
            sze = sze * this%dimsizes(I)
        enddo
    end function

    include "ncix-int.h"
    include "ncix-real.h"

    subroutine ncix_var_get_char_at_index(this, index, dim_index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer, intent(in) :: dim_index(:)
        character(len=*), intent(out) :: val
        integer, intent(out) :: status
        if (this%data_type .eq. CDF_CHAR &
            .or. this%data_type .eq. CDF_UCHAR) then
            call CDF_get_zvar_data(this%cdf%id, this%id, index, dim_index, val, status)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine

    subroutine ncix_var_get_byte_at_index(this, index, dim_index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer, intent(in) :: dim_index(:)
        integer(int8_t), intent(out) :: val
        integer, intent(out) :: status
        if (this%data_type .eq. CDF_BYTE &
            .or. this%data_type .eq. CDF_INT1 &
            .or. this%data_type .eq. CDF_UINT1) then
            call CDF_get_zvar_data(this%cdf%id, this%id, index, dim_index, val, status)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine

    subroutine ncix_var_get_epoch_at_index(this, index, dim_index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer, intent(in) :: dim_index(:)
        type(CDFEpoch), intent(out) :: val
        integer, intent(out) :: status
        if (this%data_type .eq. CDF_EPOCH) then
            call CDF_get_zvar_data(this%cdf%id, this%id, index, dim_index, val%value, status)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine
    
    subroutine ncix_var_get_epoch16_at_index(this, index, dim_index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer, intent(in) :: dim_index(:)
        type(CDFEpoch16), intent(out) :: val
        integer, intent(out) :: status
        if (this%data_type .eq. CDF_EPOCH16) then
            call CDF_get_zvar_data(this%cdf%id, this%id, index, dim_index, val%value, status)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine
    
    subroutine ncix_var_get_detailepoch_at_index(this, index, dim_index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer, intent(in) :: dim_index(:)
        type(DetailEpoch), intent(out) :: val
        type(CDFEpoch)   :: epoch_val
        type(CDFEpoch16) :: epoch16_val
        integer, intent(out) :: status
        
        if (this%data_type .eq. CDF_EPOCH) then
            call this%get_at_index(index, dim_index, epoch_val, status)
            if (status .ne. CDF_OK) return
            call epoch_val%get_details(val)
        else if (this%data_type .eq. CDF_EPOCH16) then
            call this%get_at_index(index, dim_index, epoch16_val, status)
            if (status .ne. CDF_OK) return
            call epoch16_val%get_details(val)
        else 
            status = BAD_DATA_TYPE
        endif
    end subroutine
    
#ifdef DATETIME_FORTRAN
    subroutine ncix_var_get_datetime_at_index(this, index, dim_index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer, intent(in) :: dim_index(:)
        type(datetime), intent(out) :: val
        type(DetailEpoch)   :: epoch_val
        integer, intent(out) :: status
        
        call this%get_at_index(index, dim_index, epoch_val, status)
        if (status .ne. CDF_OK) return
        val = datetime(&
            epoch_val%year,&
            epoch_val%month,&
            epoch_val%day,&
            epoch_val%hour,&
            epoch_val%minute,&
            epoch_val%second,&
            epoch_val%msecond)
    end subroutine
#endif
end module
