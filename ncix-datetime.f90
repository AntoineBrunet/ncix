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

    subroutine ncix_var_get_datetime_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        type(datetime), pointer, intent(out) :: val
        integer, intent(out) :: status

        class(DetailEpoch), allocatable :: epoch_val(:)
        type(datetime), dimension(:), pointer :: pval
        integer :: sze, I
        sze = this%get_record_buffer_size()
        call C_F_POINTER(C_LOC(val), pval, [sze])

        allocate(epoch_val(sze))
        call this%get_record(index, epoch_val, status)
        if (status .ne. CDF_OK) then
            deallocate(epoch_val)
            return
        endif
        do I=1,sze
            call epoch_val(I)%get_datetime(pval(I))
        enddo
        deallocate(epoch_val)
    end subroutine
    
    subroutine ncix_var_put_datetime_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        type(datetime), pointer, intent(in) :: val
        integer, intent(out) :: status

        type(DetailEpoch), allocatable :: epoch_val(:)
        type(datetime), dimension(:), pointer :: pval
        integer :: sze, I
        sze = this%get_record_buffer_size()
        call C_F_POINTER(C_LOC(val), pval, [sze])

        allocate(epoch_val(sze))
        do I=1,sze
            call epoch_val(I)%from_datetime(pval(I))
        enddo
        call this%put_record(index, epoch_val, status)
        deallocate(epoch_val)
        if ((status .eq. CDF_OK) .and. (index .gt. this%numrecs)) then
            call CDF_get_zvar_maxwrittenrecnum(this%cdf%id, this%id, &
                this%numrecs, status)
        endif
    end subroutine

    subroutine ncix_var_attr_get_datetime_sca(this, name, val, status)
        class(CDFVar), intent(in) :: this
        character(len=*), intent(in) :: name
        type(datetime), intent(out) :: val
        integer, intent(out) :: status
        type(DetailEpoch) :: details

        call this%get_attr(name, details, status)
        if (status .ne. CDF_OK) return
        call details%get_datetime(val)
    end subroutine
    
    subroutine ncix_var_attr_put_datetime_sca(this, name, val, status)
        class(CDFVar), intent(in) :: this
        character(len=*), intent(in) :: name
        type(datetime), intent(in) :: val
        integer, intent(out) :: status
        type(DetailEpoch) :: details

        call details%from_datetime(val)
        call this%put_attr(name, details, status)
    end subroutine

    subroutine ncix_attr_get_datetime_sca(this, name, val, status)
        class(CDF), intent(in) :: this
        character(len=*), intent(in) :: name
        type(datetime), intent(out) :: val
        integer, intent(out) :: status
        type(DetailEpoch) :: details

        call this%get_attr(name, details, status)
        if (status .ne. CDF_OK) return
        call details%get_datetime(val)
    end subroutine
    
    subroutine ncix_attr_put_datetime_sca(this, name, val, status)
        class(CDF), intent(in) :: this
        character(len=*), intent(in) :: name
        type(datetime), intent(in) :: val
        integer, intent(out) :: status
        type(DetailEpoch) :: details

        call details%from_datetime(val)
        call this%put_attr(name, details, status)
    end subroutine


#define NCIX_TYPENAME datetime
#define NCIX_TYPE type(datetime)
#define NCIX_NOATTR
#include "ncix_template.f90"
