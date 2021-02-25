module ncix
    use iso_fortran_env, int8_t => int8
    use cdf_mod, int8 => cdf_int8
    use ncix_epoch
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
        procedure, pass :: ncix_var_get_byte_at_index
        procedure, pass :: ncix_var_get_int2_at_index
        procedure, pass :: ncix_var_get_int4_at_index
        procedure, pass :: ncix_var_get_float_at_index
        procedure, pass :: ncix_var_get_double_at_index
        procedure, pass :: ncix_var_get_epoch_at_index
        procedure, pass :: ncix_var_get_epoch16_at_index
        procedure, pass :: ncix_var_get_detailepoch_at_index
        generic :: get_at_index => ncix_var_get_byte_at_index, &
                              ncix_var_get_int2_at_index, &
                              ncix_var_get_int4_at_index, &
                              ncix_var_get_epoch_at_index, &
                              ncix_var_get_epoch16_at_index, &
                              ncix_var_get_detailepoch_at_index, &
                              ncix_var_get_float_at_index, &
                              ncix_var_get_double_at_index

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

    subroutine ncix_get_var_by_name(this, name, var, status)
        class(CDF), intent(in) :: this
        character(len=*), intent(in) :: name
        type(CDFVar), intent(out) :: var
        integer, intent(out) :: status
        integer*4 CDF_get_var_num

        var%cdf = this
        status = CDF_get_var_num(this%id, name)
        var%id = status
        var%name = name
        if (status .ge. 0) status = CDF_OK
        if (status .eq. CDF_OK) call var%init(status)
    end subroutine

    subroutine ncix_get_var_by_id(this, id, var, status)
        class(CDF), intent(in) :: this
        integer, intent(in) :: id
        type(CDFVar), intent(out) :: var
        integer, intent(out) :: status

        call CDF_get_zvar_name(this%id, id, var%name, status)
        var%cdf = this
        var%id = id
        if (status .eq. CDF_OK) call var%init(status)
    end subroutine

    subroutine ncix_get_nb_var(this, nb_var, status)
        class(CDF), intent(in) :: this
        integer, intent(out) :: nb_var
        integer, intent(out) :: status
        call CDF_get_num_zvars(this%id, nb_var, status)
    end subroutine

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

    subroutine ncix_var_get_int2_at_index(this, index, dim_index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer, intent(in) :: dim_index(:)
        integer(int16), intent(out) :: val
        integer, intent(out) :: status
        if (this%data_type .eq. CDF_INT2 &
            .or. this%data_type .eq. CDF_UINT2) then
            call CDF_get_zvar_data(this%cdf%id, this%id, index, dim_index, val, status)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine

    subroutine ncix_var_get_int4_at_index(this, index, dim_index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer, intent(in) :: dim_index(:)
        integer(int32), intent(out) :: val
        integer, intent(out) :: status
        if (this%data_type .eq. CDF_INT4 &
            .or. this%data_type .eq. CDF_UINT4) then
            call CDF_get_zvar_data(this%cdf%id, this%id, index, dim_index, val, status)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine
    
    subroutine ncix_var_get_float_at_index(this, index, dim_index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer, intent(in) :: dim_index(:)
        real(real32), intent(out) :: val
        integer, intent(out) :: status
        if (this%data_type .eq. CDF_FLOAT &
            .or. this%data_type .eq. CDF_REAL4) then
            call CDF_get_zvar_data(this%cdf%id, this%id, index, dim_index, val, status)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine
    
    subroutine ncix_var_get_double_at_index(this, index, dim_index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer, intent(in) :: dim_index(:)
        real(real64), intent(out) :: val
        integer, intent(out) :: status
        if (this%data_type .eq. CDF_DOUBLE &
            .or. this%data_type .eq. CDF_REAL8 &
            .or. this%data_type .eq. CDF_EPOCH &
            .or. this%data_type .eq. CDF_EPOCH16) then
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
        else if (this%data_type .eq. CDF_EPOCH) then
            call this%get_at_index(index, dim_index, epoch16_val, status)
            if (status .ne. CDF_OK) return
            call epoch16_val%get_details(val)
        else 
            status = BAD_DATA_TYPE
        endif
    end subroutine
end module
