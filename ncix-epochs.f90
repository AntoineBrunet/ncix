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
    
    subroutine ncix_var_get_epoch_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        type(CDFEpoch), pointer, intent(out) :: val
        integer, intent(out) :: status
        
        integer, dimension(this%numdims) :: dim_index
        dim_index = 1
        if (this%data_type .eq. CDF_EPOCH) then
            call CDF_hyper_get_zvar_data(this%cdf%id, this%id, &
                index, 1, 1, &
                dim_index, this%dimsizes, dim_index, &
                val, status)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine
    
    subroutine ncix_var_put_epoch_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        type(CDFEpoch), pointer, intent(in) :: val
        integer, intent(out) :: status

        integer, dimension(this%numdims) :: dim_index
        dim_index = 1
        if (this%data_type .eq. CDF_EPOCH) then
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
    
    subroutine ncix_var_get_epoch16_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        type(CDFEpoch16), pointer, intent(out) :: val
        integer, intent(out) :: status

        integer, dimension(this%numdims) :: dim_index
        dim_index = 1
        if (this%data_type .eq. CDF_EPOCH16) then
            call CDF_hyper_get_zvar_data(this%cdf%id, this%id, &
                index, 1, 1, &
                dim_index, this%dimsizes, dim_index, &
                val, status)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine
    
    subroutine ncix_var_put_epoch16_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        type(CDFEpoch16), pointer, intent(in) :: val
        integer, intent(out) :: status

        integer, dimension(this%numdims) :: dim_index
        dim_index = 1
        if (this%data_type .eq. CDF_EPOCH16) then
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
    
    subroutine ncix_var_get_detailepoch_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        type(DetailEpoch), pointer, intent(out) :: val
        integer, intent(out) :: status

        type(CDFEpoch), allocatable :: epoch_val(:)
        type(CDFEpoch16), allocatable :: epoch16_val(:)
        type(DetailEpoch), dimension(:), pointer :: pval

        integer :: sze, I
        sze = this%get_record_buffer_size()
        call C_F_POINTER(C_LOC(val), pval, [sze])

        if (this%data_type .eq. CDF_EPOCH) then
            allocate(epoch_val(sze))
            call this%get_record(index, epoch_val, status)
            if (status .ne. CDF_OK) then
                deallocate(epoch_val)
                return
            endif
            do I=1,sze
                call epoch_val(I)%get_details(pval(I))
            enddo
            deallocate(epoch_val)
        else if (this%data_type .eq. CDF_EPOCH16) then
            allocate(epoch16_val(sze))
            call this%get_record(index, epoch16_val, status)
            if (status .ne. CDF_OK) then
                deallocate(epoch16_val)
                return
            endif
            do I=1,sze
                call epoch16_val(I)%get_details(pval(I))
            enddo
            deallocate(epoch16_val)
        else 
            status = BAD_DATA_TYPE
        endif
    end subroutine

    subroutine ncix_var_put_detailepoch_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        type(DetailEpoch), pointer, intent(in) :: val
        integer, intent(out) :: status

        type(CDFEpoch), allocatable :: epoch_val(:)
        type(CDFEpoch16), allocatable :: epoch16_val(:)
        type(DetailEpoch), dimension(:), pointer :: pval
        integer :: sze, I
        sze = this%get_record_buffer_size()
        call C_F_POINTER(C_LOC(val), pval, [sze])

        if (this%data_type .eq. CDF_EPOCH) then
            allocate(epoch_val(sze))
            do I=1,sze
                call epoch_val(I)%from_details(pval(I))
            enddo
            call this%put_record(index, epoch_val, status)
            deallocate(epoch_val)
            if (status .ne. CDF_OK) return
        else if (this%data_type .eq. CDF_EPOCH16) then
            allocate(epoch16_val(sze))
            do I=1,sze
                call epoch16_val(I)%from_details(pval(I))
            enddo
            call this%put_record(index, epoch16_val, status)
            deallocate(epoch_val)
            if (status .ne. CDF_OK) return
        else
            status = BAD_DATA_TYPE
        endif
        if ((status .eq. CDF_OK) .and. (index .gt. this%numrecs)) then
            call CDF_get_zvar_maxwrittenrecnum(this%cdf%id, this%id, &
                this%numrecs, status)
        endif
    end subroutine

#define NCIX_TYPENAME detailepoch
#define NCIX_TYPE type(DetailEpoch)
#include "ncix_records_template.f90"

#define NCIX_TYPENAME epoch16
#define NCIX_TYPE type(CDFEpoch16)
#include "ncix_records_template.f90"

#define NCIX_TYPENAME epoch
#define NCIX_TYPE type(CDFEpoch)
#include "ncix_records_template.f90"
