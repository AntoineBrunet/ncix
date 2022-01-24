
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
        integer(int32), pointer, intent(out) :: val
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
        integer(int32), target, dimension(:), intent(out) :: val
        integer, intent(out) :: status
        integer(int32), pointer :: pval
        pval => val(1)
        call ncix_var_get_int4_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_get_int4_record_mat(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), target, intent(out) :: val(:,:)
        integer, intent(out) :: status
        integer(int32), pointer :: pval
        pval => val(1,1)
        call ncix_var_get_int4_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_get_int4_record_ter(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), target, intent(out) :: val(:,:,:)
        integer, intent(out) :: status
        integer(int32), pointer :: pval
        pval => val(1,1,1)
        call ncix_var_get_int4_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_get_int4_record_qad(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), target, intent(out) :: val(:,:,:,:)
        integer, intent(out) :: status
        integer(int32), pointer :: pval
        pval => val(1,1,1,1)
        call ncix_var_get_int4_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_get_int2_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int16), pointer, intent(out) :: val
        integer, intent(out) :: status

        integer, dimension(this%numdims) :: dim_index
        dim_index = 1
        if (this%data_type .eq. CDF_INT2 &
            .or. this%data_type .eq. CDF_UINT2) then
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
        integer(int16), target, dimension(:), intent(out) :: val
        integer, intent(out) :: status
        integer(int16), pointer :: pval
        pval => val(1)
        call ncix_var_get_int2_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_get_int2_record_mat(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int16), target, intent(out) :: val(:,:)
        integer, intent(out) :: status
        integer(int16), pointer :: pval
        pval => val(1,1)
        call ncix_var_get_int2_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_get_int2_record_ter(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int16), target, intent(out) :: val(:,:,:)
        integer, intent(out) :: status
        integer(int16), pointer :: pval
        pval => val(1,1,1)
        call ncix_var_get_int2_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_get_int2_record_qad(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int16), target, intent(out) :: val(:,:,:,:)
        integer, intent(out) :: status
        integer(int16), pointer :: pval
        pval => val(1,1,1,1)
        call ncix_var_get_int2_record(this, index, pval, status)
    end subroutine

    subroutine ncix_var_put_int4_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), pointer, intent(in) :: val
        integer, intent(out) :: status

        integer, dimension(this%numdims) :: dim_index
        dim_index = 1
        if (this%data_type .eq. CDF_INT4 &
            .or. this%data_type .eq. CDF_UINT4) then
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
    
    subroutine ncix_var_put_int4_record_vec(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), target, dimension(:), intent(in) :: val
        integer, intent(out) :: status
        integer(int32), pointer :: pval
        pval => val(1)
        call ncix_var_put_int4_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_put_int4_record_mat(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), target, intent(in) :: val(:,:)
        integer, intent(out) :: status
        integer(int32), pointer :: pval
        pval => val(1,1)
        call ncix_var_put_int4_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_put_int4_record_ter(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), target, intent(in) :: val(:,:,:)
        integer, intent(out) :: status
        integer(int32), pointer :: pval
        pval => val(1,1,1)
        call ncix_var_put_int4_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_put_int4_record_qad(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), target, intent(in) :: val(:,:,:,:)
        integer, intent(out) :: status
        integer(int32), pointer :: pval
        pval => val(1,1,1,1)
        call ncix_var_put_int4_record(this, index, pval, status)
    end subroutine

    subroutine ncix_var_put_int2_record(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int16), pointer, intent(in) :: val
        integer, intent(out) :: status

        integer, dimension(this%numdims) :: dim_index
        dim_index = 1
        if (this%data_type .eq. CDF_INT2 &
            .or. this%data_type .eq. CDF_UINT2) then
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
    
    subroutine ncix_var_put_int2_record_vec(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int16), target, dimension(:), intent(in) :: val
        integer, intent(out) :: status
        integer(int16), pointer :: pval
        pval => val(1)
        call ncix_var_put_int2_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_put_int2_record_mat(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int16), target, intent(in) :: val(:,:)
        integer, intent(out) :: status
        integer(int16), pointer :: pval
        pval => val(1,1)
        call ncix_var_put_int2_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_put_int2_record_ter(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int16), target, intent(in) :: val(:,:,:)
        integer, intent(out) :: status
        integer(int16), pointer :: pval
        pval => val(1,1,1)
        call ncix_var_put_int2_record(this, index, pval, status)
    end subroutine
    
    subroutine ncix_var_put_int2_record_qad(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        integer(int16), target, intent(in) :: val(:,:,:,:)
        integer, intent(out) :: status
        integer(int16), pointer :: pval
        pval => val(1,1,1,1)
        call ncix_var_put_int2_record(this, index, pval, status)
    end subroutine


    subroutine ncix_var_add_int4_record(this, val, status)
        class(CDFVar), intent(inout) :: this
        integer(int32), pointer, intent(in) :: val
        integer, intent(out) :: status

        call ncix_var_put_int4_record(this, this%numrecs+1, val, status)
    end subroutine

    subroutine ncix_var_add_int4_record_vec(this, val, status)
        class(CDFVar), intent(inout) :: this
        integer(int32), target, dimension(:), intent(in) :: val
        integer, intent(out) :: status
        call ncix_var_put_int4_record_vec(this, this%numrecs+1, val, status)
    end subroutine

    subroutine ncix_var_add_int4_record_mat(this, val, status)
        class(CDFVar), intent(inout) :: this
        integer(int32), target, dimension(:,:), intent(in) :: val
        integer, intent(out) :: status
        call ncix_var_put_int4_record_mat(this, this%numrecs+1, val, status)
    end subroutine

    subroutine ncix_var_add_int4_record_ter(this, val, status)
        class(CDFVar), intent(inout) :: this
        integer(int32), target, dimension(:,:,:), intent(in) :: val
        integer, intent(out) :: status
        call ncix_var_put_int4_record_ter(this, this%numrecs+1, val, status)
    end subroutine

    subroutine ncix_var_add_int4_record_qad(this, val, status)
        class(CDFVar), intent(inout) :: this
        integer(int32), target, dimension(:,:,:,:), intent(in) :: val
        integer, intent(out) :: status
        call ncix_var_put_int4_record_qad(this, this%numrecs+1, val, status)
    end subroutine


    subroutine ncix_var_add_int2_record(this, val, status)
        class(CDFVar), intent(inout) :: this
        integer(int16), pointer, intent(in) :: val
        integer, intent(out) :: status

        call ncix_var_put_int2_record(this, this%numrecs+1, val, status)
    end subroutine

    subroutine ncix_var_add_int2_record_vec(this, val, status)
        class(CDFVar), intent(inout) :: this
        integer(int16), target, dimension(:), intent(in) :: val
        integer, intent(out) :: status
        call ncix_var_put_int2_record_vec(this, this%numrecs+1, val, status)
    end subroutine

    subroutine ncix_var_add_int2_record_mat(this, val, status)
        class(CDFVar), intent(inout) :: this
        integer(int16), target, dimension(:,:), intent(in) :: val
        integer, intent(out) :: status
        call ncix_var_put_int2_record_mat(this, this%numrecs+1, val, status)
    end subroutine

    subroutine ncix_var_add_int2_record_ter(this, val, status)
        class(CDFVar), intent(inout) :: this
        integer(int16), target, dimension(:,:,:), intent(in) :: val
        integer, intent(out) :: status
        call ncix_var_put_int2_record_ter(this, this%numrecs+1, val, status)
    end subroutine

    subroutine ncix_var_add_int2_record_qad(this, val, status)
        class(CDFVar), intent(inout) :: this
        integer(int16), target, dimension(:,:,:,:), intent(in) :: val
        integer, intent(out) :: status
        call ncix_var_put_int2_record_qad(this, this%numrecs+1, val, status)
    end subroutine

