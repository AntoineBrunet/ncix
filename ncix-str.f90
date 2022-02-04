    subroutine ncix_var_attr_get_str_sca(this, name, val, status)
        class(CDFVar), intent(in) :: this
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: val
        integer, intent(out) :: status

        integer*4 CDF_get_attr_num
        type(CDFEpoch) :: epoch
        type(CDFEpoch16) :: epoch16
        integer :: attr_n, dtype,nelems
        attr_n = CDF_get_attr_num(this%cdf%id, name)
        if (attr_n .lt. 0) then
            status = attr_n
            return
        endif
        call CDF_inquire_attr_zentry(this%cdf%id, attr_n, this%id, dtype, nelems, status)
        if (status .ne. CDF_OK) return
        if (dtype .eq. CDF_CHAR .or. dtype .eq. CDF_UCHAR) then
            ALLOCATE(character(len=nelems) :: val)
            call CDF_get_attr_zentry(this%cdf%id, attr_n, this%id, val, status)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine
    
    subroutine ncix_var_attr_put_str_sca(this, name, val, status)
        class(CDFVar), intent(in) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: val
        integer, intent(out) :: status
        integer*4 CDF_get_attr_num

        integer :: attr_n, dtype,nelems
        attr_n = CDF_get_attr_num(this%cdf%id, name)
        if (attr_n .eq. NO_SUCH_ATTR) then
            call CDF_create_attr(this%cdf%id, name, VARIABLE_SCOPE, attr_n, status)
            if (status .ne. CDF_OK) return
        endif
        if (attr_n .lt. 0) then
            status = attr_n
            return
        endif
        call CDF_put_attr_zentry(this%cdf%id, attr_n, this%id, CDF_CHAR, LEN(val), val, status)
    end subroutine
    
    subroutine ncix_attr_get_str_sca(this, name, val, status)
        class(CDF), intent(in) :: this
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: val
        integer, intent(out) :: status

        integer*4 CDF_get_attr_num
        type(CDFEpoch) :: epoch
        type(CDFEpoch16) :: epoch16
        integer :: attr_n, dtype,nelems
        attr_n = CDF_get_attr_num(this%id, name)
        if (attr_n .lt. 0) then
            status = attr_n
            return
        endif
        call CDF_inquire_attr_gentry(this%id, attr_n, 1, dtype, nelems, status)
        if (status .ne. CDF_OK) return
        if (dtype .eq. CDF_CHAR .or. dtype .eq. CDF_UCHAR) then
            ALLOCATE(character(len=nelems) :: val)
            call CDF_get_attr_gentry(this%id, attr_n, 1, val, status)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine
    
    subroutine ncix_attr_put_str_sca(this, name, val, status)
        class(CDF), intent(in) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: val
        integer, intent(out) :: status
        integer*4 CDF_get_attr_num

        integer :: attr_n, dtype,nelems
        attr_n = CDF_get_attr_num(this%id, name)
        if (attr_n .eq. NO_SUCH_ATTR) then
            call CDF_create_attr(this%id, name, GLOBAL_SCOPE, attr_n, status)
            if (status .ne. CDF_OK) return
        endif
        if (attr_n .lt. 0) then
            status = attr_n
            return
        endif
        call CDF_put_attr_gentry(this%id, attr_n, 1, CDF_CHAR, LEN(val), val, status)
    end subroutine
