module test_operator_unitTests_catenate
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert
    use :: strings_enclose
    use :: orbs_type_operableBitset
    private
    public :: cat_op_catenate_two_bitsets

contains
    subroutine cat_op_catenate_two_bitsets(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        character(:), allocatable :: lhs, rhs

        logical :: stat
        character(:), allocatable :: msg

        lhs = repeat("1", 32)
        rhs = repeat("1", 32)
        bitset = operable_bitset(lhs)//operable_bitset(rhs)
        call expect_equal(bitset%to_string(), lhs//rhs, &
                          enclose(lhs, "'")//" // "//enclose(rhs, "'")//" should return "//enclose(lhs//rhs, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 32)
        rhs = repeat("0", 32)
        bitset = operable_bitset(lhs)//operable_bitset(rhs)
        call expect_equal(bitset%to_string(), lhs//rhs, &
                          enclose(lhs, "'")//" // "//enclose(rhs, "'")//" should return "//enclose(lhs//rhs, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("1", 128)
        bitset = operable_bitset(lhs)//operable_bitset(rhs)
        call expect_equal(bitset%to_string(), lhs//rhs, &
                          enclose(lhs, "'")//" // "//enclose(rhs, "'")//" should return "//enclose(lhs//rhs, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("0", 128)
        bitset = operable_bitset(lhs)//operable_bitset(rhs)
        call expect_equal(bitset%to_string(), lhs//rhs, &
                          enclose(lhs, "'")//" // "//enclose(rhs, "'")//" should return "//enclose(lhs//rhs, "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine cat_op_catenate_two_bitsets
end module test_operator_unitTests_catenate
