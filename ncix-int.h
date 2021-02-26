
    subroutine ncix_var_get_int2_at_index(this, index, dim_index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer, intent(in) :: dim_index(:)
        integer(int16), intent(out) :: val
        integer, intent(out) :: status
        if (this%data_type .eq. CDF_INT2 &
            .or. this%data_type .eq. CDF_UINT2) then
            call CDF_get_zvar_data(this%cdf%id, this%id, index, dim_index, val, status)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine

    subroutine ncix_var_get_int4_at_index(this, index, dim_index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer, intent(in) :: dim_index(:)
        integer(int32), intent(out) :: val
        integer, intent(out) :: status
        if (this%data_type .eq. CDF_INT4 &
            .or. this%data_type .eq. CDF_UINT4) then
            call CDF_get_zvar_data(this%cdf%id, this%id, index, dim_index, val, status)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine
    
    subroutine ncix_var_get_int4_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), intent(out) :: val
        integer, intent(out) :: status

        integer, dimension(this%numdims) :: dim_index
        dim_index = 1
        if (this%data_type .eq. CDF_INT4 &
            .or. this%data_type .eq. CDF_UINT4) then
            call CDF_hyper_get_zvar_data(this%cdf%id, this%id, &
                index, 1, 1, &
                dim_index, this%dimsizes, dim_index, &
                val, status)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine
    
    subroutine ncix_var_get_int4_record_vec(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), dimension(:), intent(out) :: val
        integer, intent(out) :: status
        call ncix_var_get_int4_record(this, index, val(1), status)
    end subroutine
    
    subroutine ncix_var_get_int4_record_mat(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), intent(out) :: val(:,:)
        integer, intent(out) :: status
        call ncix_var_get_int4_record(this, index, val(1,1), status)
    end subroutine
    
    subroutine ncix_var_get_int4_record_ter(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), intent(out) :: val(:,:,:)
        integer, intent(out) :: status
        call ncix_var_get_int4_record(this, index, val(1,1,1), status)
    end subroutine
    
    subroutine ncix_var_get_int2_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int16), intent(out) :: val
        integer, intent(out) :: status

        integer, dimension(this%numdims) :: dim_index
        dim_index = 1
        if (this%data_type .eq. CDF_INT4 &
            .or. this%data_type .eq. CDF_UINT4) then
            call CDF_hyper_get_zvar_data(this%cdf%id, this%id, &
                index, 1, 1, &
                dim_index, this%dimsizes, dim_index, &
                val, status)
        else
            status = BAD_DATA_TYPE
        endif
    end subroutine
    
    subroutine ncix_var_get_int2_record_vec(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int16), dimension(:), intent(out) :: val
        integer, intent(out) :: status
        call ncix_var_get_int2_record(this, index, val(1), status)
    end subroutine
    
    subroutine ncix_var_get_int2_record_mat(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int16), intent(out) :: val(:,:)
        integer, intent(out) :: status
        call ncix_var_get_int2_record(this, index, val(1,1), status)
    end subroutine
    
    subroutine ncix_var_get_int2_record_ter(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int16), intent(out) :: val(:,:,:)
        integer, intent(out) :: status
        call ncix_var_get_int2_record(this, index, val(1,1,1), status)
    end subroutine
