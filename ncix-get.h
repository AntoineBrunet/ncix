    subroutine ncix_new_var(this, name, data_type, dim_sizes, var, status, &
            nonvary)
        class(CDF), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: data_type
        integer, dimension(:), intent(in) :: dim_sizes
        type(CDFVar), intent(out) :: var
        integer, intent(out) :: status
        integer, intent(in), optional :: nonvary

        integer :: rvary
        integer, dimension(size(dim_sizes)) :: dvary
        integer :: vid

        if (present(nonvary)) then
            rvary = NOVARY
        else
            rvary = VARY
        endif
        dvary = VARY
        call CDF_create_zvar(this%id, name, data_type, 1, &
            size(dim_sizes), dim_sizes, rvary, dvary, vid, status)

        var%cdf = this
        var%id = vid
        if (status .eq. CDF_OK) call var%init(status)
    end subroutine

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
