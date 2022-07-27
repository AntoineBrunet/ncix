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
        integer :: majority
    contains
        procedure :: open => ncix_open
        procedure :: create => ncix_create
        procedure :: save => ncix_save
        procedure :: close => ncix_close
        procedure, pass :: ncix_get_var_by_id
        procedure, pass :: ncix_get_var_by_name
        generic :: get_var =>  ncix_get_var_by_id, ncix_get_var_by_name
        procedure :: get_nb_var => ncix_get_nb_var
        procedure :: new_var => ncix_new_var 

#define ONLY_SCA
#define HAS_STR
#define NCIX_IFX_NAME get_attr
#define NCIX_F1 ncix_attr_get
#define NCIX_F2 
#include "ncix_ifx_template.f90"
        
#define ONLY_SCA
#define HAS_STR
#define NCIX_IFX_NAME put_attr
#define NCIX_F1 ncix_attr_put
#define NCIX_F2 
#include "ncix_ifx_template.f90"
        
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

#define NCIX_IFX_NAME get_record
#define NCIX_F1 ncix_var_get
#define NCIX_F2 record_
#include "ncix_ifx_template.f90"

#define NCIX_IFX_NAME put_record
#define NCIX_F1 ncix_var_put
#define NCIX_F2 record_
#include "ncix_ifx_template.f90"
        
#define NCIX_IFX_NAME add_record
#define NCIX_F1 ncix_var_add
#define NCIX_F2 record_
#include "ncix_ifx_template.f90"

#define ONLY_SCA
#define HAS_STR
#define NCIX_IFX_NAME get_attr
#define NCIX_F1 ncix_var_attr_get
#define NCIX_F2 
#include "ncix_ifx_template.f90"
        
#define ONLY_SCA
#define HAS_STR
#define NCIX_IFX_NAME put_attr
#define NCIX_F1 ncix_var_attr_put
#define NCIX_F2 
#include "ncix_ifx_template.f90"
        
    end type
    integer, parameter :: MAX_NCIX_OPEN_FILES = 100
    integer, dimension(MAX_NCIX_OPEN_FILES) :: ncix_open_files = -1
    integer :: ncix_nb_open_files = 0
    integer, dimension(0), parameter :: NCIX_NODIM = 1
contains

    subroutine ncix_handle_error(status)
        integer, intent(in) :: status
        character(len=CDF_STATUSTEXT_LEN) ::  message
        integer :: stat, I
        if (status .eq. CDF_OK) return
        call CDF_error(status, message, stat)
        if (status .lt. CDF_WARN) then
            write(0,*)  trim(message)
            ! Try to close all ncix files!
            do I=1, ncix_nb_open_files
                call CDF_close(ncix_open_files(I), stat)
            enddo
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
        if (ncix_nb_open_files .lt. MAX_NCIX_OPEN_FILES) then
            ncix_nb_open_files = ncix_nb_open_files + 1
            ncix_open_files(ncix_nb_open_files) = this%id
        endif
        call CDF_set_zmode(this%id, 2, status)
        if (status .ne. CDF_OK) return
        call CDF_get_majority(this%id, this%majority, status)
        if (status .ne. CDF_OK) return
    end subroutine

    subroutine ncix_create(this, filename, status)
        class(CDF), intent(inout) :: this
        character(LEN=*) , intent(in) :: filename
        integer, intent(out) :: status
        call CDF_create(filename, 0, [0], HOST_ENCODING, COLUMN_MAJOR, this%id, status)
        if (status .ne. CDF_OK) return
        if (ncix_nb_open_files .lt. MAX_NCIX_OPEN_FILES) then
            ncix_nb_open_files = ncix_nb_open_files + 1
            ncix_open_files(ncix_nb_open_files) = this%id
        endif
        call CDF_set_zmode(this%id, 2, status)
        if (status .ne. CDF_OK) return
    end subroutine

    subroutine ncix_save(this, status)
        class(CDF), intent(inout) :: this
        integer, intent(out) :: status
        integer * 4, external ::  CDFlib
        !DEC$ ATTRIBUTES C, ALIAS:'CDFlib' :: CDFlib
        status = CDFlib(SELECT_, CDF_, this%id, SAVE_, CDF_, NULL_, status)
    end subroutine

    subroutine ncix_close(this, status)
        class(CDF), intent(inout) :: this
        integer, intent(out) :: status
        integer :: I
        call CDF_close(this%id, status)
        if (status .ne. CDF_OK) return
        do I=1,ncix_nb_open_files
            if (this%id .eq. ncix_open_files(I)) then
                ncix_open_files(I) = ncix_open_files(ncix_nb_open_files)
                ncix_nb_open_files = ncix_nb_open_files - 1
                exit
            endif
        enddo
    end subroutine

    include "ncix-get.f90"

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

#include "ncix-int.f90"
#include "ncix-real.f90"
#include "ncix-epochs.f90"
#include "ncix-str.f90"
#ifdef DATETIME_FORTRAN
#include "ncix-datetime.f90"
#endif

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

end module
