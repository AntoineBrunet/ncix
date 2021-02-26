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
        real(real32), intent(out) :: val
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
        real(real32), intent(out) :: val(:)
        integer, intent(out) :: status
        call ncix_var_get_float_record(this, index, val(1), status)
    end subroutine
    
    subroutine ncix_var_get_float_record_mat(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real32), intent(out) :: val(:,:)
        integer, intent(out) :: status
        call ncix_var_get_float_record(this, index, val(1,1), status)
    end subroutine
    
    subroutine ncix_var_get_float_record_ter(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real32), intent(out) :: val(:,:,:)
        integer, intent(out) :: status
        call ncix_var_get_float_record(this, index, val(1,1,1), status)
    end subroutine
    subroutine ncix_var_get_double_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real64), intent(out) :: val
        integer, intent(out) :: status

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
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine
    
    subroutine ncix_var_get_double_record_vec(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real64), intent(out) :: val(:)
        integer, intent(out) :: status
        call ncix_var_get_double_record(this, index, val(1), status)
    end subroutine
    
    subroutine ncix_var_get_double_record_mat(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real64), intent(out) :: val(:,:)
        integer, intent(out) :: status
        call ncix_var_get_double_record(this, index, val(1,1), status)
    end subroutine
    
    subroutine ncix_var_get_double_record_ter(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        real(real64), intent(out) :: val(:,:,:)
        integer, intent(out) :: status
        call ncix_var_get_double_record(this, index, val(1,1,1), status)
    end subroutine
