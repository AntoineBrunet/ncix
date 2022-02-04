#define NCIX_GET_(sfx) EVALUATOR(ncix_var_get, NCIX_TYPENAME, sfx)
#define NCIX_PUT_(sfx) EVALUATOR(ncix_var_put, NCIX_TYPENAME, sfx)
#define NCIX_ADD_(sfx) EVALUATOR(ncix_var_add, NCIX_TYPENAME, sfx)

    subroutine NCIX_GET_(record_sca)(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        NCIX_TYPE, target, intent(out) :: val
        integer, intent(out) :: status
        NCIX_TYPE, pointer :: pval
        if (this%get_record_buffer_size() .gt. 1) then 
            status = BAD_ARGUMENT
            return
        endif
        pval => val
        call NCIX_GET_(record)(this, index, pval, status)
    end subroutine

    subroutine NCIX_GET_(record_vec)(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        NCIX_TYPE, target, intent(out) :: val(:)
        integer, intent(out) :: status
        NCIX_TYPE, pointer :: pval
        if (this%get_record_buffer_size() .gt. SIZE(val)) then 
            status = BAD_ARGUMENT
            return
        endif
        pval => val(1)
        call NCIX_GET_(record)(this, index, pval, status)
    end subroutine
    
    subroutine NCIX_GET_(record_mat)(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        NCIX_TYPE, target, intent(out) :: val(:,:)
        integer, intent(out) :: status
        NCIX_TYPE, pointer :: pval
        if (this%get_record_buffer_size() .gt. SIZE(val)) then 
            status = BAD_ARGUMENT
            return
        endif
        pval => val(1,1)
        call NCIX_GET_(record)(this, index, pval, status)
    end subroutine
    
    subroutine NCIX_GET_(record_ter)(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        NCIX_TYPE, target, intent(out) :: val(:,:,:)
        integer, intent(out) :: status
        NCIX_TYPE, pointer :: pval
        if (this%get_record_buffer_size() .gt. SIZE(val)) then 
            status = BAD_ARGUMENT
            return
        endif
        pval => val(1,1,1)
        call NCIX_GET_(record)(this, index, pval, status)
    end subroutine
    
    subroutine NCIX_GET_(record_qad)(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        NCIX_TYPE, target, intent(out) :: val(:,:,:,:)
        integer, intent(out) :: status
        NCIX_TYPE, pointer :: pval
        if (this%get_record_buffer_size() .gt. SIZE(val)) then 
            status = BAD_ARGUMENT
            return
        endif
        pval => val(1,1,1,1)
        call NCIX_GET_(record)(this, index, pval, status)
    end subroutine

    subroutine NCIX_PUT_(record_sca)(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        NCIX_TYPE, target, intent(in) :: val
        integer, intent(out) :: status
        NCIX_TYPE, pointer :: pval
        if (this%get_record_buffer_size() .gt. 1) then 
            status = BAD_ARGUMENT
            return
        endif
        pval => val
        call NCIX_PUT_(record)(this, index, pval, status)
    end subroutine

    subroutine NCIX_PUT_(record_vec)(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        NCIX_TYPE, target, intent(in) :: val(:)
        integer, intent(out) :: status
        NCIX_TYPE, pointer :: pval
        if (this%get_record_buffer_size() .gt. SIZE(val)) then 
            status = BAD_ARGUMENT
            return
        endif
        pval => val(1)
        call NCIX_PUT_(record)(this, index, pval, status)
    end subroutine
    
    subroutine NCIX_PUT_(record_mat)(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        NCIX_TYPE, target, intent(in) :: val(:,:)
        integer, intent(out) :: status
        NCIX_TYPE, pointer :: pval
        if (this%get_record_buffer_size() .gt. SIZE(val)) then 
            status = BAD_ARGUMENT
            return
        endif
        pval => val(1,1)
        call NCIX_PUT_(record)(this, index, pval, status)
    end subroutine
    
    subroutine NCIX_PUT_(record_ter)(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        NCIX_TYPE, target, intent(in) :: val(:,:,:)
        integer, intent(out) :: status
        NCIX_TYPE, pointer :: pval
        if (this%get_record_buffer_size() .gt. SIZE(val)) then 
            status = BAD_ARGUMENT
            return
        endif
        pval => val(1,1,1)
        call NCIX_PUT_(record)(this, index, pval, status)
    end subroutine
    
    subroutine NCIX_PUT_(record_qad)(this, index, val, status)
        class(CDFVar), intent(inout) :: this
        integer, intent(in) :: index
        NCIX_TYPE, target, intent(in) :: val(:,:,:,:)
        integer, intent(out) :: status
        NCIX_TYPE, pointer :: pval
        if (this%get_record_buffer_size() .gt. SIZE(val)) then 
            status = BAD_ARGUMENT
            return
        endif
        pval => val(1,1,1,1)
        call NCIX_PUT_(record)(this, index, pval, status)
    end subroutine

    subroutine NCIX_ADD_(record_sca)(this, val, status)
        class(CDFVar), intent(inout) :: this
        NCIX_TYPE, target, intent(in) :: val
        integer, intent(out) :: status

        call NCIX_PUT_(record_sca)(this, this%numrecs+1, val, status)
    end subroutine

    subroutine NCIX_ADD_(record_vec)(this, val, status)
        class(CDFVar), intent(inout) :: this
        NCIX_TYPE, target, dimension(:), intent(in) :: val
        integer, intent(out) :: status
        call NCIX_PUT_(record)_vec(this, this%numrecs+1, val, status)
    end subroutine

    subroutine NCIX_ADD_(record_mat)(this, val, status)
        class(CDFVar), intent(inout) :: this
        NCIX_TYPE, target, dimension(:,:), intent(in) :: val
        integer, intent(out) :: status
        call NCIX_PUT_(record)_mat(this, this%numrecs+1, val, status)
    end subroutine

    subroutine NCIX_ADD_(record_ter)(this, val, status)
        class(CDFVar), intent(inout) :: this
        NCIX_TYPE, target, dimension(:,:,:), intent(in) :: val
        integer, intent(out) :: status
        call NCIX_PUT_(record)_ter(this, this%numrecs+1, val, status)
    end subroutine

    subroutine NCIX_ADD_(record_qad)(this, val, status)
        class(CDFVar), intent(inout) :: this
        NCIX_TYPE, target, dimension(:,:,:,:), intent(in) :: val
        integer, intent(out) :: status
        call NCIX_PUT_(record)_qad(this, this%numrecs+1, val, status)
    end subroutine

#undef NCIX_TYPENAME
#undef NCIX_TYPE
