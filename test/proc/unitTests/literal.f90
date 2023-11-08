module test_proc_unitTests_literal
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert
    use :: orbs_type_operableBitset
    use :: orbs_proc_literal
    implicit none
    private
    public :: one_returns_1_in_binary_representation
    public :: zero_returns_0_in_binary_representation

contains
    subroutine one_returns_1_in_binary_representation(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(operable_bitset) :: bitset, one
        character(:), allocatable :: str_one

        bitset = operable_bitset("0010101001010001010101010101000101")
        one = orbs_one(mold=bitset)

        call expect_true(all([one%bits() == 34, one%bit_count() == 1, one%test(0)]), &
                         "orbs_one(mold) should return a bitset with the same digits of the mold and only LSB set to 1", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        str_one = one%to_string()

        call expect_equal(str_one, "0000000000000000000000000000000001", &
                          "orbs_one(mold) should return 1 in binary representation with the same digits of the mold", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        bitset = operable_bitset("00")
        one = orbs_one(mold=bitset)
        str_one = one%to_string()
        call expect_equal(str_one, "01", &
                          "orbs_one(mold) should return 1 in binary representation with the same digits of the mold", &
                          stat, output_message=msg)
    end subroutine one_returns_1_in_binary_representation

    subroutine zero_returns_0_in_binary_representation(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(operable_bitset) :: bitset, zero
        character(:), allocatable :: str_zero

        bitset = operable_bitset("0010101001010001010101010101000101")
        zero = orbs_zero(mold=bitset)

        call expect_true(all([zero%bits() == 34, zero%bit_count() == 0]), &
                         "orbs_one(mold) should return a bitset with the same digits of the mold and no bits are set", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        str_zero = zero%to_string()

        call expect_equal(str_zero, "0000000000000000000000000000000000", &
                          "orbs_one(mold) should return 0 in binary representation with the same digits of the mold", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        bitset = operable_bitset("10")
        zero = orbs_zero(mold=bitset)
        str_zero = zero%to_string()
        call expect_equal(str_zero, "00", &
                          "orbs_one(mold) should return 0 in binary representation with the same digits of the mold", &
                          stat, output_message=msg)
    end subroutine zero_returns_0_in_binary_representation
end module test_proc_unitTests_literal
