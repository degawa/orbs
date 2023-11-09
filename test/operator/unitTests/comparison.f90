module test_operator_unitTests_comparison
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert
    use :: strings_enclose
    use :: orbs_type_operableBitset
    implicit none
    private
    public :: eq_op_returns_true_if_both_sides_have_same_bitset
    public :: eq_op_returns_false_if_both_sides_have_different_bitset
    public :: neq_op_returns_true_if_both_sides_have_different_bitset
    public :: neq_op_returns_false_if_both_sides_have_same_bitset
    public :: gt_op_returns_true_if_lhs_is_greater_than_rhs
    public :: gt_op_returns_false_if_lhs_is_less_than_or_equal_to_rhs
    public :: ge_op_returns_true_if_lhs_is_greater_than_or_equal_to_rhs
    public :: ge_op_returns_false_if_lhs_is_less_than_rhs
    public :: lt_op_returns_true_if_lhs_is_less_than_rhs
    public :: lt_op_returns_false_if_lhs_is_greater_than_or_equal_to_rhs
    public :: le_op_returns_true_if_lhs_is_less_than_or_equal_to_rhs
    public :: le_op_returns_false_if_lhs_is_greater_than_rhs

contains
    subroutine eq_op_returns_true_if_both_sides_have_same_bitset(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: lhs, rhs
        logical :: stat
        character(:), allocatable :: msg

        lhs = repeat("1", 64)
        rhs = repeat("1", 64)
        call expect_true(operable_bitset(lhs) == operable_bitset(rhs), &
                         enclose(lhs, "'")//" == "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 64)
        rhs = repeat("0", 64)
        call expect_true(operable_bitset(lhs) == operable_bitset(rhs), &
                         enclose(lhs, "'")//" == "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("10", 32)
        rhs = repeat("10", 32)
        call expect_true(operable_bitset(lhs) == operable_bitset(rhs), &
                         enclose(lhs, "'")//" == "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("1", 128)
        call expect_true(operable_bitset(lhs) == operable_bitset(rhs), &
                         enclose(lhs, "'")//" == "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128)
        rhs = repeat("0", 128)
        call expect_true(operable_bitset(lhs) == operable_bitset(rhs), &
                         enclose(lhs, "'")//" == "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("101", 42)
        rhs = repeat("101", 42)
        call expect_true(operable_bitset(lhs) == operable_bitset(rhs), &
                         enclose(lhs, "'")//" == "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine eq_op_returns_true_if_both_sides_have_same_bitset

    subroutine eq_op_returns_false_if_both_sides_have_different_bitset(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: lhs, rhs
        logical :: stat
        character(:), allocatable :: msg

        lhs = repeat("1", 64)
        rhs = repeat("0", 64)
        call expect_false(operable_bitset(lhs) == operable_bitset(rhs), &
                          enclose(lhs, "'")//" == "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 64)
        rhs = repeat("1", 64)
        call expect_false(operable_bitset(lhs) == operable_bitset(rhs), &
                          enclose(lhs, "'")//" == "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("10", 32)
        rhs = repeat("01", 32)
        call expect_false(operable_bitset(lhs) == operable_bitset(rhs), &
                          enclose(lhs, "'")//" == "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("0", 128)
        call expect_false(operable_bitset(lhs) == operable_bitset(rhs), &
                          enclose(lhs, "'")//" == "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("0", 128)
        call expect_false(operable_bitset(lhs) == operable_bitset(rhs), &
                          enclose(lhs, "'")//" == "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("101", 42)
        rhs = repeat("010", 42)
        call expect_false(operable_bitset(lhs) == operable_bitset(rhs), &
                          enclose(lhs, "'")//" == "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 7)
        rhs = repeat("1", 6)
        call expect_false(operable_bitset(lhs) == operable_bitset(rhs), &
                          enclose(lhs, "'")//" == "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine eq_op_returns_false_if_both_sides_have_different_bitset

    subroutine neq_op_returns_true_if_both_sides_have_different_bitset(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: lhs, rhs
        logical :: stat
        character(:), allocatable :: msg

        lhs = repeat("1", 64)
        rhs = repeat("0", 64)
        call expect_true(operable_bitset(lhs) /= operable_bitset(rhs), &
                         enclose(lhs, "'")//" /= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 64)
        rhs = repeat("1", 64)
        call expect_true(operable_bitset(lhs) /= operable_bitset(rhs), &
                         enclose(lhs, "'")//" /= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("10", 32)
        rhs = repeat("01", 32)
        call expect_true(operable_bitset(lhs) /= operable_bitset(rhs), &
                         enclose(lhs, "'")//" /= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("0", 128)
        call expect_true(operable_bitset(lhs) /= operable_bitset(rhs), &
                         enclose(lhs, "'")//" /= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("0", 128)
        call expect_true(operable_bitset(lhs) /= operable_bitset(rhs), &
                         enclose(lhs, "'")//" /= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("101", 42)
        rhs = repeat("010", 42)
        call expect_true(operable_bitset(lhs) /= operable_bitset(rhs), &
                         enclose(lhs, "'")//" /= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 7)
        rhs = repeat("1", 6)
        call expect_true(operable_bitset(lhs) /= operable_bitset(rhs), &
                         enclose(lhs, "'")//" /= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine neq_op_returns_true_if_both_sides_have_different_bitset

    subroutine neq_op_returns_false_if_both_sides_have_same_bitset(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: lhs, rhs
        logical :: stat
        character(:), allocatable :: msg

        lhs = repeat("1", 64)
        rhs = repeat("1", 64)
        call expect_false(operable_bitset(lhs) /= operable_bitset(rhs), &
                          enclose(lhs, "'")//" /= "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 64)
        rhs = repeat("0", 64)
        call expect_false(operable_bitset(lhs) /= operable_bitset(rhs), &
                          enclose(lhs, "'")//" /= "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("10", 32)
        rhs = repeat("10", 32)
        call expect_false(operable_bitset(lhs) /= operable_bitset(rhs), &
                          enclose(lhs, "'")//" /= "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("1", 128)
        call expect_false(operable_bitset(lhs) /= operable_bitset(rhs), &
                          enclose(lhs, "'")//" /= "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128)
        rhs = repeat("0", 128)
        call expect_false(operable_bitset(lhs) /= operable_bitset(rhs), &
                          enclose(lhs, "'")//" /= "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("101", 42)
        rhs = repeat("101", 42)
        call expect_false(operable_bitset(lhs) /= operable_bitset(rhs), &
                          enclose(lhs, "'")//" /= "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine neq_op_returns_false_if_both_sides_have_same_bitset

    subroutine gt_op_returns_true_if_lhs_is_greater_than_rhs(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: lhs, rhs
        logical :: stat
        character(:), allocatable :: msg

        lhs = "00000000000000000000000000000001"
        rhs = "00000000000000000000000000000000"
        call expect_true(operable_bitset(lhs) > operable_bitset(rhs), &
                         enclose(lhs, "'")//" > "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "11111111111111111111111111111111"
        rhs = "11111111111111111111111111111110"
        call expect_true(operable_bitset(lhs) > operable_bitset(rhs), &
                         enclose(lhs, "'")//" > "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128); lhs(128:128) = "1"
        rhs = repeat("0", 128)
        call expect_true(operable_bitset(lhs) > operable_bitset(rhs), &
                         enclose(lhs, "'")//" > "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("1", 128); rhs(128:128) = "0"
        call expect_true(operable_bitset(lhs) > operable_bitset(rhs), &
                         enclose(lhs, "'")//" > "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine gt_op_returns_true_if_lhs_is_greater_than_rhs

    subroutine gt_op_returns_false_if_lhs_is_less_than_or_equal_to_rhs(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: lhs, rhs
        logical :: stat
        character(:), allocatable :: msg

        lhs = "00000000000000000000000000000000"
        rhs = "00000000000000000000000000000000"
        call expect_false(operable_bitset(lhs) > operable_bitset(rhs), &
                          enclose(lhs, "'")//" > "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "00000000000000000000000000000000"
        rhs = "00000000000000000000000000000001"
        call expect_false(operable_bitset(lhs) > operable_bitset(rhs), &
                          enclose(lhs, "'")//" > "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "11111111111111111111111111111111"
        rhs = "11111111111111111111111111111111"
        call expect_false(operable_bitset(lhs) > operable_bitset(rhs), &
                          enclose(lhs, "'")//" > "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "11111111111111111111111111111110"
        rhs = "11111111111111111111111111111111"
        call expect_false(operable_bitset(lhs) > operable_bitset(rhs), &
                          enclose(lhs, "'")//" > "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128)
        rhs = repeat("0", 128)
        call expect_false(operable_bitset(lhs) > operable_bitset(rhs), &
                          enclose(lhs, "'")//" > "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128)
        rhs = repeat("0", 128); rhs(128:128) = "1"
        call expect_false(operable_bitset(lhs) > operable_bitset(rhs), &
                          enclose(lhs, "'")//" > "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("1", 128)
        call expect_false(operable_bitset(lhs) > operable_bitset(rhs), &
                          enclose(lhs, "'")//" > "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128); lhs(128:128) = "0"
        rhs = repeat("1", 128)
        call expect_false(operable_bitset(lhs) > operable_bitset(rhs), &
                          enclose(lhs, "'")//" > "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine gt_op_returns_false_if_lhs_is_less_than_or_equal_to_rhs

    subroutine ge_op_returns_true_if_lhs_is_greater_than_or_equal_to_rhs(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: lhs, rhs
        logical :: stat
        character(:), allocatable :: msg

        lhs = "00000000000000000000000000000001"
        rhs = "00000000000000000000000000000000"
        call expect_true(operable_bitset(lhs) >= operable_bitset(rhs), &
                         enclose(lhs, "'")//" >= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "00000000000000000000000000000000"
        rhs = "00000000000000000000000000000000"
        call expect_true(operable_bitset(lhs) >= operable_bitset(rhs), &
                         enclose(lhs, "'")//" >= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "11111111111111111111111111111111"
        rhs = "11111111111111111111111111111110"
        call expect_true(operable_bitset(lhs) >= operable_bitset(rhs), &
                         enclose(lhs, "'")//" >= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "11111111111111111111111111111111"
        rhs = "11111111111111111111111111111111"
        call expect_true(operable_bitset(lhs) >= operable_bitset(rhs), &
                         enclose(lhs, "'")//" >= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128); lhs(128:128) = "1"
        rhs = repeat("0", 128)
        call expect_true(operable_bitset(lhs) >= operable_bitset(rhs), &
                         enclose(lhs, "'")//" >= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128)
        rhs = repeat("0", 128)
        call expect_true(operable_bitset(lhs) >= operable_bitset(rhs), &
                         enclose(lhs, "'")//" >= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("1", 128); rhs(128:128) = "0"
        call expect_true(operable_bitset(lhs) >= operable_bitset(rhs), &
                         enclose(lhs, "'")//" >= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("1", 128)
        call expect_true(operable_bitset(lhs) >= operable_bitset(rhs), &
                         enclose(lhs, "'")//" >= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine ge_op_returns_true_if_lhs_is_greater_than_or_equal_to_rhs

    subroutine ge_op_returns_false_if_lhs_is_less_than_rhs(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: lhs, rhs
        logical :: stat
        character(:), allocatable :: msg

        lhs = "00000000000000000000000000000000"
        rhs = "00000000000000000000000000000001"
        call expect_false(operable_bitset(lhs) >= operable_bitset(rhs), &
                          enclose(lhs, "'")//" >= "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "11111111111111111111111111111110"
        rhs = "11111111111111111111111111111111"
        call expect_false(operable_bitset(lhs) >= operable_bitset(rhs), &
                          enclose(lhs, "'")//" >= "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128)
        rhs = repeat("0", 128); rhs(128:128) = "1"
        call expect_false(operable_bitset(lhs) >= operable_bitset(rhs), &
                          enclose(lhs, "'")//" >= "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128); lhs(128:128) = "0"
        rhs = repeat("1", 128)
        call expect_false(operable_bitset(lhs) >= operable_bitset(rhs), &
                          enclose(lhs, "'")//" >= "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine ge_op_returns_false_if_lhs_is_less_than_rhs

    subroutine lt_op_returns_true_if_lhs_is_less_than_rhs(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: lhs, rhs
        logical :: stat
        character(:), allocatable :: msg

        lhs = "00000000000000000000000000000000"
        rhs = "00000000000000000000000000000001"
        call expect_true(operable_bitset(lhs) < operable_bitset(rhs), &
                         enclose(lhs, "'")//" < "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "11111111111111111111111111111110"
        rhs = "11111111111111111111111111111111"
        call expect_true(operable_bitset(lhs) < operable_bitset(rhs), &
                         enclose(lhs, "'")//" < "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128)
        rhs = repeat("0", 128); rhs(128:128) = "1"
        call expect_true(operable_bitset(lhs) < operable_bitset(rhs), &
                         enclose(lhs, "'")//" < "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128); lhs(128:128) = "0"
        rhs = repeat("1", 128)
        call expect_true(operable_bitset(lhs) < operable_bitset(rhs), &
                         enclose(lhs, "'")//" < "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine lt_op_returns_true_if_lhs_is_less_than_rhs

    subroutine lt_op_returns_false_if_lhs_is_greater_than_or_equal_to_rhs(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: lhs, rhs
        logical :: stat
        character(:), allocatable :: msg

        lhs = "00000000000000000000000000000000"
        rhs = "00000000000000000000000000000000"
        call expect_false(operable_bitset(lhs) < operable_bitset(rhs), &
                          enclose(lhs, "'")//" < "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "00000000000000000000000000000001"
        rhs = "00000000000000000000000000000000"
        call expect_false(operable_bitset(lhs) < operable_bitset(rhs), &
                          enclose(lhs, "'")//" < "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "11111111111111111111111111111111"
        rhs = "11111111111111111111111111111111"
        call expect_false(operable_bitset(lhs) < operable_bitset(rhs), &
                          enclose(lhs, "'")//" < "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "11111111111111111111111111111111"
        rhs = "11111111111111111111111111111110"
        call expect_false(operable_bitset(lhs) < operable_bitset(rhs), &
                          enclose(lhs, "'")//" < "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128)
        rhs = repeat("0", 128)
        call expect_false(operable_bitset(lhs) < operable_bitset(rhs), &
                          enclose(lhs, "'")//" < "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128); lhs(128:128) = "1"
        rhs = repeat("0", 128)
        call expect_false(operable_bitset(lhs) < operable_bitset(rhs), &
                          enclose(lhs, "'")//" < "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("1", 128)
        call expect_false(operable_bitset(lhs) < operable_bitset(rhs), &
                          enclose(lhs, "'")//" < "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("1", 128); rhs(128:128) = "0"
        call expect_false(operable_bitset(lhs) < operable_bitset(rhs), &
                          enclose(lhs, "'")//" < "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine lt_op_returns_false_if_lhs_is_greater_than_or_equal_to_rhs

    subroutine le_op_returns_true_if_lhs_is_less_than_or_equal_to_rhs(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: lhs, rhs
        logical :: stat
        character(:), allocatable :: msg

        lhs = "00000000000000000000000000000000"
        rhs = "00000000000000000000000000000001"
        call expect_true(operable_bitset(lhs) <= operable_bitset(rhs), &
                         enclose(lhs, "'")//" <= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "00000000000000000000000000000000"
        rhs = "00000000000000000000000000000000"
        call expect_true(operable_bitset(lhs) <= operable_bitset(rhs), &
                         enclose(lhs, "'")//" <= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "11111111111111111111111111111110"
        rhs = "11111111111111111111111111111111"
        call expect_true(operable_bitset(lhs) <= operable_bitset(rhs), &
                         enclose(lhs, "'")//" <= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "11111111111111111111111111111111"
        rhs = "11111111111111111111111111111111"
        call expect_true(operable_bitset(lhs) <= operable_bitset(rhs), &
                         enclose(lhs, "'")//" <= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128)
        rhs = repeat("0", 128); rhs(128:128) = "1"
        call expect_true(operable_bitset(lhs) <= operable_bitset(rhs), &
                         enclose(lhs, "'")//" <= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128)
        rhs = repeat("0", 128)
        call expect_true(operable_bitset(lhs) <= operable_bitset(rhs), &
                         enclose(lhs, "'")//" <= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128); lhs(128:128) = "0"
        rhs = repeat("1", 128)
        call expect_true(operable_bitset(lhs) <= operable_bitset(rhs), &
                         enclose(lhs, "'")//" <= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("1", 128)
        call expect_true(operable_bitset(lhs) <= operable_bitset(rhs), &
                         enclose(lhs, "'")//" <= "//enclose(rhs, "'")//" should return .true.", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine le_op_returns_true_if_lhs_is_less_than_or_equal_to_rhs

    subroutine le_op_returns_false_if_lhs_is_greater_than_rhs(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: lhs, rhs
        logical :: stat
        character(:), allocatable :: msg

        lhs = "00000000000000000000000000000001"
        rhs = "00000000000000000000000000000000"
        call expect_false(operable_bitset(lhs) <= operable_bitset(rhs), &
                          enclose(lhs, "'")//" <= "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = "11111111111111111111111111111111"
        rhs = "11111111111111111111111111111110"
        call expect_false(operable_bitset(lhs) <= operable_bitset(rhs), &
                          enclose(lhs, "'")//" <= "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("0", 128); lhs(128:128) = "1"
        rhs = repeat("0", 128)
        call expect_false(operable_bitset(lhs) <= operable_bitset(rhs), &
                          enclose(lhs, "'")//" <= "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        lhs = repeat("1", 128)
        rhs = repeat("1", 128); rhs(128:128) = "0"
        call expect_false(operable_bitset(lhs) <= operable_bitset(rhs), &
                          enclose(lhs, "'")//" <= "//enclose(rhs, "'")//" should return .false.", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine le_op_returns_false_if_lhs_is_greater_than_rhs
end module test_operator_unitTests_comparison
