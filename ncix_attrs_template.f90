#define NCIX_VATTR_GET_(sfx) EVALUATOR(ncix_var_attr_get, NCIX_TYPENAME, sfx)

    subroutine NCIX_VATTR_GET_(sca)(this, name, val, status)
        class(CDFVar), intent(in) :: this
        character(len=*), intent(in) :: name
        NCIX_TYPE, intent(out) :: val
        integer, intent(out) :: status
        integer*4 CDF_get_attr_num

        integer :: attr_n, dtype,nelems
        attr_n = CDF_get_attr_num(this%cdf%id, name)
        if (attr_n .lt. 0) then
            status = attr_n
            return
        endif
        call CDF_inquire_attr_zentry(this%cdf%id, attr_n, this%id, dtype, nelems, status)
        if (status .ne. CDF_OK) return

        if (NCIX_TL) then
            if (nelems .ne. 1) then
                status = UNSUPPORTED_OPERATION
            else
                call CDF_get_attr_zentry(this%cdf%id, attr_n,this%id, val, status)
            endif
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine

#define NCIX_VATTR_PUT_(sfx) EVALUATOR(ncix_var_attr_put, NCIX_TYPENAME, sfx)

    subroutine NCIX_VATTR_PUT_(sca)(this, name, val, status)
        class(CDFVar), intent(in) :: this
        character(len=*), intent(in) :: name
        NCIX_TYPE, intent(in) :: val
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
        call CDF_put_attr_zentry(this%cdf%id, attr_n, this%id, NCIX_DTYPE, 1, val, status)
    end subroutine

#define NCIX_GATTR_GET_(sfx) EVALUATOR(ncix_attr_get, NCIX_TYPENAME, sfx)

    subroutine NCIX_GATTR_GET_(sca)(this, name, val, status)
        class(CDF), intent(in) :: this
        character(len=*), intent(in) :: name
        NCIX_TYPE, intent(out) :: val
        integer, intent(out) :: status
        integer*4 CDF_get_attr_num

        integer :: attr_n, dtype,nelems
        attr_n = CDF_get_attr_num(this%id, name)
        if (attr_n .lt. 0) then
            status = attr_n
            return
        endif
        call CDF_inquire_attr_gentry(this%id, attr_n, 1, dtype, nelems, status)
        if (status .ne. CDF_OK) return

        if (NCIX_TL) then
            if (nelems .ne. 1) then
                status = UNSUPPORTED_OPERATION
            else
                call CDF_get_attr_gentry(this%id, attr_n, 1, val, status)
            endif
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine

#define NCIX_GATTR_PUT_(sfx) EVALUATOR(ncix_attr_put, NCIX_TYPENAME, sfx)

    subroutine NCIX_GATTR_PUT_(sca)(this, name, val, status)
        class(CDF), intent(in) :: this
        character(len=*), intent(in) :: name
        NCIX_TYPE, intent(in) :: val
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
        call CDF_put_attr_gentry(this%id, attr_n, 1, NCIX_DTYPE, 1, val, status)
    end subroutine
