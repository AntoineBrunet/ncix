
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
    
    subroutine ncix_var_get_int4_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), pointer, intent(out) :: val
        integer, intent(out) :: status

        integer, dimension(this%numdims) :: dim_index
        dim_index = 1
        if (this%data_type .eq. CDF_INT4 &
            .or. this%data_type .eq. CDF_UINT4) then
            call CDF_hyper_get_zvar_data(this%cdf%id, this%id, &
                index, 1, 1, &
                dim_index, this%dimsizes, dim_index, &
                val, status)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine
    
    subroutine ncix_var_get_int2_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int16), pointer, intent(out) :: val
        integer, intent(out) :: status

        integer, dimension(this%numdims) :: dim_index
        dim_index = 1
        if (this%data_type .eq. CDF_INT2 &
            .or. this%data_type .eq. CDF_UINT2) then
            call CDF_hyper_get_zvar_data(this%cdf%id, this%id, &
                index, 1, 1, &
                dim_index, this%dimsizes, dim_index, &
                val, status)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine
    
    subroutine ncix_var_put_int4_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), pointer, intent(in) :: val
        integer, intent(out) :: status

        integer, dimension(this%numdims) :: dim_index
        dim_index = 1
        if (this%data_type .eq. CDF_INT4 &
            .or. this%data_type .eq. CDF_UINT4) then
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
    
    subroutine ncix_var_put_int2_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int16), pointer, intent(in) :: val
        integer, intent(out) :: status

        integer, dimension(this%numdims) :: dim_index
        dim_index = 1
        if (this%data_type .eq. CDF_INT2 &
            .or. this%data_type .eq. CDF_UINT2) then
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
    
#define NCIX_TYPENAME int2
#define NCIX_TYPE integer(int16)
#define NCIX_TL dtype .eq. CDF_INT2 .or. dtype .eq. CDF_UINT2
#define NCIX_DTYPE CDF_INT2
#include "ncix_template.f90"

#define NCIX_TYPENAME int4
#define NCIX_TYPE integer(int32)
#define NCIX_TL dtype .eq. CDF_INT4 .or. dtype .eq. CDF_UINT4
#define NCIX_DTYPE CDF_INT4
#include "ncix_template.f90"
