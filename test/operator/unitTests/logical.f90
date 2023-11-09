module test_operator_unitTests_logical
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert
    use :: orbs_type_operableBitset
    implicit none
    private
    public :: and_op_compute_bitwise_logical_and
    public :: or_op_compute_bitwise_logical_or
    public :: xor_op_compute_bitwise_logical_exclusive_or
    public :: not_op_compute_bitwise_logical_complement
    public :: andnot_op_compute_logical_and_of_a_and_not_b

contains
    subroutine and_op_compute_bitwise_logical_and(error)
        use :: strings_enclose
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(operable_bitset) :: bitset
        character(:), allocatable :: lhs, rhs, res

        lhs = "1111111111111111"
        rhs = "1111111111111111"
        res = "1111111111111111"
        bitset = operable_bitset(lhs) .and. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .and. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "1111111111111111"
        rhs = "0000000000000000"
        res = "0000000000000000"
        bitset = operable_bitset(lhs) .and. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .and. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "0000000000000000"
        rhs = "0000000000000000"
        res = "0000000000000000"
        bitset = operable_bitset(lhs) .and. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .and. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "1010101010101010"
        rhs = "1111111111111111"
        res = "1010101010101010"
        bitset = operable_bitset(lhs) .and. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .and. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("1", 128)
        res = repeat("1", 128)
        bitset = operable_bitset(lhs) .and. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .and. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("0", 128)
        res = repeat("0", 128)
        bitset = operable_bitset(lhs) .and. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .and. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128)
        rhs = repeat("0", 128)
        res = repeat("0", 128)
        bitset = operable_bitset(lhs) .and. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .and. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("10", 64)
        rhs = repeat("1", 128)
        res = repeat("10", 64)
        bitset = operable_bitset(lhs) .and. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .and. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine and_op_compute_bitwise_logical_and

    subroutine or_op_compute_bitwise_logical_or(error)
        use :: strings_enclose
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(operable_bitset) :: bitset
        character(:), allocatable :: lhs, rhs, res

        lhs = "1111111111111111"
        rhs = "1111111111111111"
        res = "1111111111111111"
        bitset = operable_bitset(lhs) .or. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .or. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "1111111111111111"
        rhs = "0000000000000000"
        res = "1111111111111111"
        bitset = operable_bitset(lhs) .or. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .or. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "0000000000000000"
        rhs = "0000000000000000"
        res = "0000000000000000"
        bitset = operable_bitset(lhs) .or. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .or. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "1010101010101010"
        rhs = "0101010101010101"
        res = "1111111111111111"
        bitset = operable_bitset(lhs) .or. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .or. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("1", 128)
        res = repeat("1", 128)
        bitset = operable_bitset(lhs) .or. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .or. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("0", 128)
        res = repeat("1", 128)
        bitset = operable_bitset(lhs) .or. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .or. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128)
        rhs = repeat("0", 128)
        res = repeat("0", 128)
        bitset = operable_bitset(lhs) .or. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .or. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("10", 64)
        rhs = repeat("01", 64)
        res = repeat("1", 128)
        bitset = operable_bitset(lhs) .or. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .or. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine or_op_compute_bitwise_logical_or

    subroutine xor_op_compute_bitwise_logical_exclusive_or(error)
        use :: strings_enclose
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(operable_bitset) :: bitset
        character(:), allocatable :: lhs, rhs, res

        lhs = "1111111111111111"
        rhs = "1111111111111111"
        res = "0000000000000000"
        bitset = operable_bitset(lhs) .xor.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .xor. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "1111111111111111"
        rhs = "0000000000000000"
        res = "1111111111111111"
        bitset = operable_bitset(lhs) .xor.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .xor. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "0000000000000000"
        rhs = "1111111111111111"
        res = "1111111111111111"
        bitset = operable_bitset(lhs) .xor.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .xor. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "0000000000000000"
        rhs = "0000000000000000"
        res = "0000000000000000"
        bitset = operable_bitset(lhs) .xor.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .xor. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "1010101010101010"
        rhs = "0101010101010101"
        res = "1111111111111111"
        bitset = operable_bitset(lhs) .xor.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .xor. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("1", 128)
        res = repeat("0", 128)
        bitset = operable_bitset(lhs) .xor.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .xor. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("0", 128)
        res = repeat("1", 128)
        bitset = operable_bitset(lhs) .xor.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .xor. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128)
        rhs = repeat("1", 128)
        res = repeat("1", 128)
        bitset = operable_bitset(lhs) .xor.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .xor. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128)
        rhs = repeat("0", 128)
        res = repeat("0", 128)
        bitset = operable_bitset(lhs) .xor.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .xor. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("10", 64)
        rhs = repeat("01", 64)
        res = repeat("1", 128)
        bitset = operable_bitset(lhs) .xor.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .xor. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine xor_op_compute_bitwise_logical_exclusive_or

    subroutine not_op_compute_bitwise_logical_complement(error)
        use :: strings_enclose
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(operable_bitset) :: bitset
        character(:), allocatable :: rhs, res

        rhs = "1111111111111111"
        res = "0000000000000000"
        bitset = .not. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          " .not. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        rhs = "0000000000000000"
        res = "1111111111111111"
        bitset = .not. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          " .not. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        rhs = "1010101010101010"
        res = "0101010101010101"
        bitset = .not. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          " .not. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        rhs = repeat("1", 128)
        res = repeat("0", 128)
        bitset = .not. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          " .not. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        rhs = repeat("0", 128)
        res = repeat("1", 128)
        bitset = .not. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          " .not. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        rhs = repeat("10", 64)
        res = repeat("01", 64)
        bitset = .not. operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          " .not. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine not_op_compute_bitwise_logical_complement

    subroutine andnot_op_compute_logical_and_of_a_and_not_b(error)
        use :: strings_enclose
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(operable_bitset) :: bitset
        character(:), allocatable :: lhs, rhs, res

        lhs = "1111111111111111"
        rhs = "1111111111111111"
        res = "0000000000000000"
        bitset = operable_bitset(lhs) .andnot.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .andnot. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "1111111111111111"
        rhs = "0000000000000000"
        res = "1111111111111111"
        bitset = operable_bitset(lhs) .andnot.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .andnot. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "0000000000000000"
        rhs = "0000000000000000"
        res = "0000000000000000"
        bitset = operable_bitset(lhs) .andnot.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .andnot. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "0000000000000000"
        rhs = "1111111111111111"
        res = "0000000000000000"
        bitset = operable_bitset(lhs) .andnot.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .andnot. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "1010101010101010"
        rhs = "0000000000000000"
        res = "1010101010101010"
        bitset = operable_bitset(lhs) .andnot.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .andnot. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("1", 128)
        res = repeat("0", 128)
        bitset = operable_bitset(lhs) .andnot.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .andnot. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("0", 128)
        res = repeat("1", 128)
        bitset = operable_bitset(lhs) .andnot.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .andnot. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128)
        rhs = repeat("1", 128)
        res = repeat("0", 128)
        bitset = operable_bitset(lhs) .andnot.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .andnot. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128)
        rhs = repeat("0", 128)
        res = repeat("0", 128)
        bitset = operable_bitset(lhs) .andnot.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .andnot. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("10", 64)
        rhs = repeat("0", 128)
        res = repeat("10", 64)
        bitset = operable_bitset(lhs) .andnot.operable_bitset(rhs)
        call expect_equal(bitset%to_string(), res, &
                          enclose(lhs, "'")//" .andnot. "//enclose(rhs, "'")//" should return "//enclose(res, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine andnot_op_compute_logical_and_of_a_and_not_b
end module test_operator_unitTests_logical
