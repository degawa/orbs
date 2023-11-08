module orbs
    use :: orbs_type_operableBitset
    use :: orbs_proc_error, orbs_get_error_message => get_error_message
    implicit none
    private
    ! type
    public :: operable_bitset

    ! procedures
    public :: orbs_get_error_message
end module orbs
