module test_operator_unitTests_arithmetic
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert
    use :: strings_enclose
    use :: orbs_type_operableBitset
    implicit none
    private
    public :: add_op_returns_bitset_having_value_of_result_of_addition
    public :: sub_op_returns_bitset_having_value_of_result_of_subtraction

contains
    subroutine add_op_returns_bitset_having_value_of_result_of_addition(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        character(:), allocatable :: lhs, rhs, add
        logical :: stat
        character(:), allocatable :: msg

        lhs = "00000000011110110000"
        rhs = "00010101010100010000"
        add = "00010101110011000000"
        bitset = operable_bitset(lhs) + operable_bitset(rhs)
        call expect_equal(bitset%to_string(), add, &
                          enclose(lhs, "'")//" + "//enclose(rhs, "'")//" should return "//enclose(add, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "00000000000000000000000000000001"
        rhs = "00000000000000000000000000000000"
        add = "00000000000000000000000000000001"
        bitset = operable_bitset(lhs) + operable_bitset(rhs)
        call expect_equal(bitset%to_string(), add, &
                          enclose(lhs, "'")//" + "//enclose(rhs, "'")//" should return "//enclose(add, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "00000000000000000000000000000000"
        rhs = "00000000000000000000000000000001"
        add = "00000000000000000000000000000001"
        bitset = operable_bitset(lhs) + operable_bitset(rhs)
        call expect_equal(bitset%to_string(), add, &
                          enclose(lhs, "'")//" + "//enclose(rhs, "'")//" should return "//enclose(add, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "00000000000000000000000000000001"
        rhs = "00000000000000000000000000000001"
        add = "00000000000000000000000000000010"
        bitset = operable_bitset(lhs) + operable_bitset(rhs)
        call expect_equal(bitset%to_string(), add, &
                          enclose(lhs, "'")//" + "//enclose(rhs, "'")//" should return "//enclose(add, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        ! overflow
        lhs = "11111111111111111111111111111111"
        rhs = "00000000000000000000000000000001"
        add = "00000000000000000000000000000000"
        bitset = operable_bitset(lhs) + operable_bitset(rhs)
        call expect_equal(bitset%to_string(), add, &
                          enclose(lhs, "'")//" + "//enclose(rhs, "'")//" should return "//enclose(add, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128); lhs(128:128) = "1"
        rhs = repeat("0", 128)
        add = repeat("0", 128); add(128:128) = "1"
        bitset = operable_bitset(lhs) + operable_bitset(rhs)
        call expect_equal(bitset%to_string(), add, &
                          enclose(lhs, "'")//" + "//enclose(rhs, "'")//" should return "//enclose(add, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128)
        rhs = repeat("0", 128); rhs(128:128) = "1"
        add = repeat("0", 128); add(128:128) = "1"
        bitset = operable_bitset(lhs) + operable_bitset(rhs)
        call expect_equal(bitset%to_string(), add, &
                          enclose(lhs, "'")//" + "//enclose(rhs, "'")//" should return "//enclose(add, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128); lhs(128:128) = "1"
        rhs = repeat("0", 128); rhs(128:128) = "1"
        add = repeat("0", 128); add(127:127) = "1"
        bitset = operable_bitset(lhs) + operable_bitset(rhs)
        call expect_equal(bitset%to_string(), add, &
                          enclose(lhs, "'")//" + "//enclose(rhs, "'")//" should return "//enclose(add, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        ! overflow
        lhs = repeat("1", 128)
        rhs = repeat("0", 128); rhs(128:128) = "1"
        add = repeat("0", 128)
        bitset = operable_bitset(lhs) + operable_bitset(rhs)
        call expect_equal(bitset%to_string(), add, &
                          enclose(lhs, "'")//" + "//enclose(rhs, "'")//" should return "//enclose(add, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine add_op_returns_bitset_having_value_of_result_of_addition

    subroutine sub_op_returns_bitset_having_value_of_result_of_subtraction(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        character(:), allocatable :: lhs, rhs, sub
        logical :: stat
        character(:), allocatable :: msg

        lhs = "00010101110011000000"
        rhs = "00000000011110110000"
        sub = "00010101010100010000"
        bitset = operable_bitset(lhs) - operable_bitset(rhs)
        call expect_equal(bitset%to_string(), sub, &
                          enclose(lhs, "'")//" - "//enclose(rhs, "'")//" should return "//enclose(sub, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "00000000000000000000000000000001"
        rhs = "00000000000000000000000000000000"
        sub = "00000000000000000000000000000001"
        bitset = operable_bitset(lhs) - operable_bitset(rhs)
        call expect_equal(bitset%to_string(), sub, &
                          enclose(lhs, "'")//" - "//enclose(rhs, "'")//" should return "//enclose(sub, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "00000000000000000000000000000001"
        rhs = "00000000000000000000000000000001"
        sub = "00000000000000000000000000000000"
        bitset = operable_bitset(lhs) - operable_bitset(rhs)
        call expect_equal(bitset%to_string(), sub, &
                          enclose(lhs, "'")//" - "//enclose(rhs, "'")//" should return "//enclose(sub, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        ! underflow
        lhs = "00000000000000000000000000000000"
        rhs = "00000000000000000000000000000001"
        sub = "11111111111111111111111111111111"
        bitset = operable_bitset(lhs) - operable_bitset(rhs)
        call expect_equal(bitset%to_string(), sub, &
                          enclose(lhs, "'")//" - "//enclose(rhs, "'")//" should return "//enclose(sub, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128); lhs(128:128) = "1"
        rhs = repeat("0", 128)
        sub = repeat("0", 128); sub(128:128) = "1"
        bitset = operable_bitset(lhs) - operable_bitset(rhs)
        call expect_equal(bitset%to_string(), sub, &
                          enclose(lhs, "'")//" - "//enclose(rhs, "'")//" should return "//enclose(sub, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128); lhs(128:128) = "1"
        rhs = repeat("0", 128); rhs(128:128) = "1"
        sub = repeat("0", 128)
        bitset = operable_bitset(lhs) - operable_bitset(rhs)
        call expect_equal(bitset%to_string(), sub, &
                          enclose(lhs, "'")//" - "//enclose(rhs, "'")//" should return "//enclose(sub, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128); lhs(127:127) = "1"
        rhs = repeat("0", 128); rhs(128:128) = "1"
        sub = repeat("0", 128); sub(128:128) = "1"
        bitset = operable_bitset(lhs) - operable_bitset(rhs)
        call expect_equal(bitset%to_string(), sub, &
                          enclose(lhs, "'")//" - "//enclose(rhs, "'")//" should return "//enclose(sub, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        ! underflow
        lhs = repeat("0", 128)
        rhs = repeat("0", 128); rhs(128:128) = "1"
        sub = repeat("1", 128)
        bitset = operable_bitset(lhs) - operable_bitset(rhs)
        call expect_equal(bitset%to_string(), sub, &
                          enclose(lhs, "'")//" - "//enclose(rhs, "'")//" should return "//enclose(sub, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine sub_op_returns_bitset_having_value_of_result_of_subtraction
end module test_operator_unitTests_arithmetic
