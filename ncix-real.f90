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
    
    subroutine ncix_var_get_float_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real32), pointer, intent(out) :: val
        integer, intent(out) :: status

        integer, dimension(this%numdims) :: dim_index
        dim_index = 1
        if (this%data_type .eq. CDF_FLOAT &
            .or. this%data_type .eq. CDF_REAL4) then
            call CDF_hyper_get_zvar_data(this%cdf%id, this%id, &
                index, 1, 1, &
                dim_index, this%dimsizes, dim_index, &
                val, status)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine


    subroutine ncix_var_get_double_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real64), pointer, intent(out) :: val
        real(real32), dimension(:), allocatable :: buffer
        real(real64), dimension(:), pointer :: pval 
        integer, intent(out) :: status

        integer :: sze
        integer, dimension(this%numdims) :: dim_index
        dim_index = 1
        if (this%data_type .eq. CDF_DOUBLE &
            .or. this%data_type .eq. CDF_REAL8 &
            .or. this%data_type .eq. CDF_EPOCH &
            .or. this%data_type .eq. CDF_EPOCH16) then
            call CDF_hyper_get_zvar_data(this%cdf%id, this%id, &
                index, 1, 1, &
                dim_index, this%dimsizes, dim_index, &
                val, status)
        elseif (this%data_type .eq. CDF_FLOAT &
            .or. this%data_type .eq. CDF_REAL4) then
            sze = this%get_record_buffer_size()
            call C_F_POINTER(C_LOC(val), pval, [sze])
            allocate(buffer(sze))
            call CDF_hyper_get_zvar_data(this%cdf%id, this%id, &
                index, 1, 1, &
                dim_index, this%dimsizes, dim_index, &
                buffer, status)
            pval = buffer
            deallocate(buffer)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine
    
    subroutine ncix_var_put_float_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real32), pointer, intent(in) :: val
        integer, intent(out) :: status

        integer, dimension(this%numdims) :: dim_index
        dim_index = 1
        if (this%data_type .eq. CDF_FLOAT &
            .or. this%data_type .eq. CDF_REAL4) then
            call CDF_hyper_put_zvar_data(this%cdf%id, this%id, &
                index, 1, 1, &
                dim_index, this%dimsizes, dim_index, &
                val, status)
        else
            status = BAD_DATA_TYPE
        endif
        if ((status .eq. CDF_OK) .and. (index .gt. this%numrecs)) then
            call CDF_get_zvar_maxwrittenrecnum(this%cdf%id, this%id, &
                this%numrecs, status)
        endif
    end subroutine
    

    subroutine ncix_var_put_double_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real64), pointer, intent(in) :: val
        integer, intent(out) :: status

        integer, dimension(this%numdims) :: dim_index
        dim_index = 1
        if (this%data_type .eq. CDF_DOUBLE &
            .or. this%data_type .eq. CDF_REAL8 &
            .or. this%data_type .eq. CDF_EPOCH &
            .or. this%data_type .eq. CDF_EPOCH16) then
            call CDF_hyper_put_zvar_data(this%cdf%id, this%id, &
                index, 1, 1, &
                dim_index, this%dimsizes, dim_index, &
                val, status)
        else
            status = BAD_DATA_TYPE
        endif
        if ((status .eq. CDF_OK) .and. (index .gt. this%numrecs)) then
            call CDF_get_zvar_maxwrittenrecnum(this%cdf%id, this%id, &
                this%numrecs, status)
        endif
    end subroutine

#define NCIX_TYPENAME float
#define NCIX_TYPE real(real32)
#define NCIX_TL dtype .eq. CDF_FLOAT .or. dtype .eq. CDF_REAL4
#define NCIX_DTYPE CDF_REAL4
#include "ncix_template.f90"

#define NCIX_TYPENAME double
#define NCIX_TYPE real(real64)
#define NCIX_TL dtype .eq. CDF_DOUBLE .or. dtype .eq. CDF_REAL8
#define NCIX_DTYPE CDF_REAL8
#include "ncix_template.f90"


