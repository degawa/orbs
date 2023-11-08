module orbs_proc_literal
    use :: orbs_type_operableBitset
    implicit none
    private
    public :: orbs_one
    public :: orbs_zero

contains
    !>Returns a bitset literal representing 1 in binary representation.
    function orbs_one(mold) result(new_bitset)
        type(operable_bitset), intent(in) :: mold
            !! mold of new bitset insatane
        type(operable_bitset) :: new_bitset
            !! new bitset instance representing 1

        new_bitset = operable_bitset(mold%bits())
        call new_bitset%set(0)
    end function orbs_one

    !>Returns a bitset literal representing 0 in binary representation.
    function orbs_zero(mold) result(new_bitset)
        type(operable_bitset), intent(in) :: mold
            !! mold of new bitset insatane
        type(operable_bitset) :: new_bitset
            !! new bitset instance representing 1

        new_bitset = operable_bitset(mold%bits())
    end function orbs_zero
end module orbs_proc_literal
