module ncix_epoch
    use iso_fortran_env, int8_t => int8
    use cdf_mod, int8 => cdf_int8
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
    end type DetailEpoch

    type :: CDFEpoch16
        real(real64) :: value(2)
        contains
        procedure :: get_details => ncix_epoch16_get_details
    end type CDFEpoch16

    type :: CDFEpoch
        real(real64) :: value
        contains
        procedure :: get_details => ncix_epoch_get_details
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
end module
