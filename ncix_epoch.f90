module ncix_epoch
    use iso_fortran_env, int8_t => int8
    use cdf_mod, int8 => cdf_int8
#ifdef DATETIME_FORTRAN
    use datetime_module, only:datetime
#endif
    implicit none

    type :: DetailEpoch
        integer :: year
        integer :: month
        integer :: day
        integer :: hour
        integer :: minute
        integer :: second
        integer :: msecond
        integer :: usecond
        integer :: nsecond
        integer :: psecond
        contains
            procedure, pass :: ncix_detail_get_epoch
            procedure, pass :: ncix_detail_get_epoch16
            generic :: get_epoch => ncix_detail_get_epoch, &
                                ncix_detail_get_epoch16
            procedure, pass :: ncix_detail_from_epoch
            procedure, pass :: ncix_detail_from_epoch16
            generic :: from_epoch => ncix_detail_from_epoch, &
                                ncix_detail_from_epoch16
#ifdef DATETIME_FORTRAN
            procedure :: get_datetime => ncix_detail_get_datetime
            procedure :: from_datetime => ncix_detail_from_datetime
#endif
    end type DetailEpoch

    type :: CDFEpoch16
        real(real64) :: value(2)
        contains
        procedure :: get_details => ncix_epoch16_get_details
        procedure :: from_details => ncix_epoch16_compute
    end type CDFEpoch16

    type :: CDFEpoch
        real(real64) :: value
        contains
        procedure :: get_details => ncix_epoch_get_details
        procedure :: from_details => ncix_epoch_compute
    end type CDFEpoch
contains
    subroutine ncix_epoch_get_details(this, details)
        class(CDFEpoch), intent(in) :: this
        type(DetailEpoch), intent(out) :: details
        call EPOCH_breakdown(this%value, &
            details%year, &
            details%month, &
            details%day, &
            details%hour, &
            details%minute, &
            details%second, &
            details%msecond)
        details%usecond = 0.0d0
        details%nsecond = 0.0d0
        details%psecond = 0.0d0
    end subroutine

    subroutine ncix_epoch_compute(this, details)
        class(CDFEpoch), intent(inout) :: this
        type(DetailEpoch), intent(in) :: details
        call compute_EPOCH(&
            details%year, &
            details%month, &
            details%day, &
            details%hour, &
            details%minute, &
            details%second, &
            details%msecond, &
            this%value)
    end subroutine

    subroutine ncix_epoch16_get_details(this, details)
        class(CDFEpoch16), intent(in) :: this
        type(DetailEpoch), intent(out) :: details
        call EPOCH16_breakdown(this%value, &
            details%year, &
            details%month, &
            details%day, &
            details%hour, &
            details%minute, &
            details%second, &
            details%msecond, &
            details%usecond, &
            details%nsecond, &
            details%psecond)
    end subroutine
    
    subroutine ncix_epoch16_compute(this, details)
        class(CDFEpoch16), intent(inout) :: this
        type(DetailEpoch), intent(in) :: details
        call compute_EPOCH16(&
            details%year, &
            details%month, &
            details%day, &
            details%hour, &
            details%minute, &
            details%second, &
            details%msecond, &
            details%usecond, &
            details%nsecond, &
            details%psecond, &
            this%value)
    end subroutine

    subroutine ncix_detail_get_epoch(this, epoch)
        class(DetailEpoch), intent(in) :: this
        class(CDFEpoch), intent(out) :: epoch
        call epoch%from_details(this)
    end subroutine
    
    subroutine ncix_detail_get_epoch16(this, epoch16)
        class(DetailEpoch), intent(in) :: this
        class(CDFEpoch16), intent(out) :: epoch16
        call epoch16%from_details(this)
    end subroutine
    
    subroutine ncix_detail_from_epoch(this, epoch)
        class(DetailEpoch), intent(out) :: this
        class(CDFEpoch), intent(in) :: epoch
        call epoch%get_details(this)
    end subroutine
    
    subroutine ncix_detail_from_epoch16(this, epoch16)
        class(DetailEpoch), intent(out) :: this
        class(CDFEpoch16), intent(in) :: epoch16
        call epoch16%get_details(this)
    end subroutine

#ifdef DATETIME_FORTRAN
    subroutine ncix_detail_get_datetime(this, date)
        class(DetailEpoch), intent(in) :: this
        type(datetime), intent(out) :: date
        date = datetime(&
            this%year,&
            this%month,&
            this%day,&
            this%hour,&
            this%minute,&
            this%second,&
            this%msecond)
    end subroutine
    
    subroutine ncix_detail_from_datetime(this, date)
        class(DetailEpoch), intent(out) :: this
        type(datetime), intent(in) :: date
        this%year    = date%getYear()
        this%month   = date%getMonth()
        this%day     = date%getDay()
        this%hour    = date%getHour()
        this%minute  = date%getMinute()
        this%second  = date%getSecond()
        this%msecond = date%getMillisecond()
        this%usecond = 0
        this%nsecond = 0
        this%psecond = 0

    end subroutine
#endif
end module
