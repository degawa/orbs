module orbs
    use :: orbs_type_operableBitset
    use :: orbs_proc_literal
    implicit none
    private
    ! type
    public :: operable_bitset

    ! procedures
    public :: orbs_one
    public :: orbs_zero
    public :: orbs_get_error_message
end module orbs
