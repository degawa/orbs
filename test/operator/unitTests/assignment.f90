module test_operator_unitTests_assignment
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert
    use :: strings_enclose
    use :: orbs_type_operableBitset
    implicit none
    private
    public :: assign_string_assigns_rhs_string_as_bitset

contains
    subroutine assign_string_assigns_rhs_string_as_bitset(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        character(:), allocatable :: rhs
        logical :: stat
        character(:), allocatable :: msg

        rhs = "1111000010101010"
        bitset = rhs
        call expect_equal(bitset%to_string(), rhs, &
                          "bitset="//enclose(rhs, "'")//" should assign "//enclose(rhs, "'")//" to bitset", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        rhs = repeat("1", 64)
        bitset = rhs
        call expect_equal(bitset%to_string(), rhs, &
                          "bitset="//enclose(rhs, "'")//" should assign "//enclose(rhs, "'")//" to bitset", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        rhs = repeat("1", 128)
        bitset = rhs
        call expect_equal(bitset%to_string(), rhs, &
                          "bitset="//enclose(rhs, "'")//" should assign "//enclose(rhs, "'")//" to bitset", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine assign_string_assigns_rhs_string_as_bitset
end module test_operator_unitTests_assignment
