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
