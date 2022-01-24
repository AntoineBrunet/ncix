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

    subroutine ncix_var_get_float_record_vec(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real32), pointer, intent(out) :: val(:)
        integer, intent(out) :: status
        real(real32), pointer :: pval
        pval => val(1)
        call ncix_var_get_float_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_get_float_record_mat(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real32), target, intent(out) :: val(:,:)
        integer, intent(out) :: status
        real(real32), pointer :: pval
        pval => val(1,1)
        call ncix_var_get_float_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_get_float_record_ter(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real32), target, intent(out) :: val(:,:,:)
        integer, intent(out) :: status
        real(real32), pointer :: pval
        pval => val(1,1,1)
        call ncix_var_get_float_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_get_float_record_qad(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real32), target, intent(out) :: val(:,:,:,:)
        integer, intent(out) :: status
        real(real32), pointer :: pval
        pval => val(1,1,1,1)
        call ncix_var_get_float_record(this, index, pval, status)
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
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine
    
    subroutine ncix_var_get_double_record_vec(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real64), target, intent(out) :: val(:)
        integer, intent(out) :: status
        real(real64), pointer :: pval
        pval => val(1)
        call ncix_var_get_double_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_get_double_record_mat(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real64), target, intent(out) :: val(:,:)
        integer, intent(out) :: status
        real(real64), pointer :: pval
        pval => val(1,1)
        call ncix_var_get_double_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_get_double_record_ter(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real64), target, intent(out) :: val(:,:,:)
        integer, intent(out) :: status
        real(real64), pointer :: pval
        pval => val(1,1,1)
        call ncix_var_get_double_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_get_double_record_qad(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real64), target, intent(out) :: val(:,:,:,:)
        integer, intent(out) :: status
        real(real64), pointer :: pval
        pval => val(1,1,1,1)
        call ncix_var_get_double_record(this, index, pval, status)
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
    
    subroutine ncix_var_put_float_record_vec(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real32), target, intent(in) :: val(:)
        integer, intent(out) :: status
        real(real32), pointer :: pval
        pval => val(1)
        call ncix_var_put_float_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_put_float_record_mat(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real32), target, intent(in) :: val(:,:)
        integer, intent(out) :: status
        real(real32), pointer :: pval
        pval => val(1,1)
        call ncix_var_put_float_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_put_float_record_ter(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real32), target, intent(in) :: val(:,:,:)
        integer, intent(out) :: status
        real(real32), pointer :: pval
        pval => val(1,1,1)
        call ncix_var_put_float_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_put_float_record_qad(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real32), target, intent(in) :: val(:,:,:,:)
        integer, intent(out) :: status
        real(real32), pointer :: pval
        pval => val(1,1,1,1)
        call ncix_var_put_float_record(this, index, pval, status)
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
    
    subroutine ncix_var_put_double_record_vec(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real64), target, intent(in) :: val(:)
        integer, intent(out) :: status
        real(real64), pointer :: pval
        pval => val(1)
        call ncix_var_put_double_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_put_double_record_mat(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real64), target, intent(in) :: val(:,:)
        integer, intent(out) :: status
        real(real64), pointer :: pval
        pval => val(1,1)
        call ncix_var_put_double_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_put_double_record_ter(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real64), target, intent(in) :: val(:,:,:)
        integer, intent(out) :: status
        real(real64), pointer :: pval
        pval => val(1,1,1)
        call ncix_var_put_double_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_put_double_record_qad(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real64), target, intent(in) :: val(:,:,:,:)
        integer, intent(out) :: status
        real(real64), pointer :: pval
        pval => val(1,1,1,1)
        call ncix_var_put_double_record(this, index, pval, status)
    end subroutine


    subroutine ncix_var_add_float_record(this, val, status)
        class(CDFVar), intent(inout) :: this
        real(real32), pointer, intent(in) :: val
        integer, intent(out) :: status

        call ncix_var_put_float_record(this, this%numrecs+1, val, status)
    end subroutine

    subroutine ncix_var_add_float_record_vec(this, val, status)
        class(CDFVar), intent(inout) :: this
        real(real32), target, dimension(:), intent(in) :: val
        integer, intent(out) :: status
        call ncix_var_put_float_record_vec(this, this%numrecs+1, val, status)
    end subroutine

    subroutine ncix_var_add_float_record_mat(this, val, status)
        class(CDFVar), intent(inout) :: this
        real(real32), target, dimension(:,:), intent(in) :: val
        integer, intent(out) :: status
        call ncix_var_put_float_record_mat(this, this%numrecs+1, val, status)
    end subroutine

    subroutine ncix_var_add_float_record_ter(this, val, status)
        class(CDFVar), intent(inout) :: this
        real(real32), target, dimension(:,:,:), intent(in) :: val
        integer, intent(out) :: status
        call ncix_var_put_float_record_ter(this, this%numrecs+1, val, status)
    end subroutine

    subroutine ncix_var_add_float_record_qad(this, val, status)
        class(CDFVar), intent(inout) :: this
        real(real32), target, dimension(:,:,:,:), intent(in) :: val
        integer, intent(out) :: status
        call ncix_var_put_float_record_qad(this, this%numrecs+1, val, status)
    end subroutine


    subroutine ncix_var_add_double_record(this, val, status)
        class(CDFVar), intent(inout) :: this
        real(real64), pointer, intent(in) :: val
        integer, intent(out) :: status

        call ncix_var_put_double_record(this, this%numrecs+1, val, status)
    end subroutine

    subroutine ncix_var_add_double_record_vec(this, val, status)
        class(CDFVar), intent(inout) :: this
        real(real64), target, dimension(:), intent(in) :: val
        integer, intent(out) :: status
        call ncix_var_put_double_record_vec(this, this%numrecs+1, val, status)
    end subroutine

    subroutine ncix_var_add_double_record_mat(this, val, status)
        class(CDFVar), intent(inout) :: this
        real(real64), target, dimension(:,:), intent(in) :: val
        integer, intent(out) :: status
        call ncix_var_put_double_record_mat(this, this%numrecs+1, val, status)
    end subroutine

    subroutine ncix_var_add_double_record_ter(this, val, status)
        class(CDFVar), intent(inout) :: this
        real(real64), target, dimension(:,:,:), intent(in) :: val
        integer, intent(out) :: status
        call ncix_var_put_double_record_ter(this, this%numrecs+1, val, status)
    end subroutine

    subroutine ncix_var_add_double_record_qad(this, val, status)
        class(CDFVar), intent(inout) :: this
        real(real64), target, dimension(:,:,:,:), intent(in) :: val
        integer, intent(out) :: status
        call ncix_var_put_double_record_qad(this, this%numrecs+1, val, status)
    end subroutine

