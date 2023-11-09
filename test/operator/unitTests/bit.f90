module test_operator_unitTests_bit
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert
    use :: strings_enclose
    use :: orbs_type_operableBitset
    implicit none
    private
    public :: shift_op_returns_bitset_having_value_of_result_of_shifting

contains
    subroutine shift_op_returns_bitset_having_value_of_result_of_shifting(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        character(:), allocatable :: bits, shifted
        logical :: stat
        character(:), allocatable :: msg

        bits = "11110000"
        shifted = "11110000"
        bitset = operable_bitset(bits) .shift.0
        call expect_equal(bitset%to_string(), shifted, &
                          "shift of "//enclose(bits, "'")//" to "//to_string(0)//" bits should return "// &
                          enclose(shifted, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        bits = "11110000"
        shifted = "11100000"
        bitset = operable_bitset(bits) .shift.1
        call expect_equal(bitset%to_string(), shifted, &
                          "left shift of "//enclose(bits, "'")//" to 1 bits should return "// &
                          enclose(shifted, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        bits = "11110000"
        shifted = "00000000"
        bitset = operable_bitset(bits) .shift.4
        call expect_equal(bitset%to_string(), shifted, &
                          "left shift of "//enclose(bits, "'")//" to 4 bits should return "// &
                          enclose(shifted, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        bits = "11110000"
        shifted = "01111000"
        bitset = operable_bitset(bits) .shift.-1
        call expect_equal(bitset%to_string(), shifted, &
                          "right shift of "//enclose(bits, "'")//" to 1 bits should return "// &
                          enclose(shifted, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        bits = "11110000"
        shifted = "00001111"
        bitset = operable_bitset(bits) .shift.-4
        call expect_equal(bitset%to_string(), shifted, &
                          "right shift of "//enclose(bits, "'")//" to 4 bits should return "// &
                          enclose(shifted, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        bits = repeat("1", 64)//repeat("0", 64)
        shifted = "1"//repeat("0", 127)
        bitset = operable_bitset(bits) .shift.63
        call expect_equal(bitset%to_string(), shifted, &
                          "left shift of "//enclose(bits, "'")//" to 63 bits should return "// &
                          enclose(shifted, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        bits = repeat("1", 64)//repeat("0", 64)
        shifted = repeat("0", 63)//repeat("1", 64)//"0"
        bitset = operable_bitset(bits) .shift.-63
        call expect_equal(bitset%to_string(), shifted, &
                          "right shift of "//enclose(bits, "'")//" to 63 bits should return "// &
                          enclose(shifted, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine shift_op_returns_bitset_having_value_of_result_of_shifting
end module test_operator_unitTests_bit
