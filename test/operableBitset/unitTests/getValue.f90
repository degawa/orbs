module test_operableBitset_unitTests_getValue
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert
    use :: orbs_type_operableBitset, only:operable_bitset
    implicit none
    private
    public :: value_returns_1_when_the_bit_is_1
    public :: value_returns_0_when_the_bit_is_0
    public :: to_string_returns_bitset_in_string
    public :: extract_returns_specified_range_of_bitset

contains
    subroutine value_returns_1_when_the_bit_is_1(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        !                         5432109876543210
        bitset = operable_bitset("1011010010101110")

        call expect_equal(bitset%value(1), 1, &
                          "value(1) for '1011010010101110' should return 1 in integer", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitset%value(2), 1, &
                          "value(2) for '1011010010101110' should return 1 in integer", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitset%value(3), 1, &
                          "value(3) for '1011010010101110' should return 1 in integer", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitset%value(5), 1, &
                          "value(5) for '1011010010101110' should return 1 in integer", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitset%value(7), 1, &
                          "value(7) for '1011010010101110' should return 1 in integer", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitset%value(10), 1, &
                          "value(10) for '1011010010101110' should return 1 in integer", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitset%value(12), 1, &
                          "value(12) for '1011010010101110' should return 1 in integer", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitset%value(13), 1, &
                          "value(13) for '1011010010101110' should return 1 in integer", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitset%value(15), 1, &
                          "value(15) for '1011010010101110' should return 1 in integer", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine value_returns_1_when_the_bit_is_1

    subroutine value_returns_0_when_the_bit_is_0(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        !                         5432109876543210
        bitset = operable_bitset("1011010010101110")

        call expect_equal(bitset%value(0), 0, &
                          "value(0) for '1011010010101110' should return 0 in integer", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitset%value(4), 0, &
                          "value(4) for '1011010010101110' should return 0 in integer", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitset%value(6), 0, &
                          "value(6) for '1011010010101110' should return 0 in integer", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitset%value(8), 0, &
                          "value(8) for '1011010010101110' should return 0 in integer", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitset%value(9), 0, &
                          "value(9) for '1011010010101110' should return 0 in integer", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitset%value(11), 0, &
                          "value(11) for '1011010010101110' should return 0 in integer", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitset%value(14), 0, &
                          "value(14) for '1011010010101110' should return 0 in integer", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine value_returns_0_when_the_bit_is_0

    subroutine to_string_returns_bitset_in_string(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        bitset = operable_bitset("1111111111111111")
        call expect_equal(bitset%to_string(), "1111111111111111", &
                          "to_string() for '1111111111111111' should return '1111111111111111' in string", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        bitset = operable_bitset("1001110101101")
        call expect_equal(bitset%to_string(), "1001110101101", &
                          "to_string() for '1001110101101' should return '1001110101101' in string", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        bitset = operable_bitset("0000000000000000000000000000000000000000000000000000000000000000")
        call expect_equal(bitset%to_string(), "0000000000000000000000000000000000000000000000000000000000000000", &
                          "to_string() for '0000000000000000000000000000000000000000000000000000000000000000' "// &
                          "should return '0000000000000000000000000000000000000000000000000000000000000000' in string", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine to_string_returns_bitset_in_string

    subroutine extract_returns_specified_range_of_bitset(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset, subset
        logical :: stat
        character(:), allocatable :: msg

        !                         765432109876543210987654321098765432109876543210
        bitset = operable_bitset("101101101010001110100011101010010010010010101110")

        subset = bitset%extract(10, 25)
        call expect_equal(subset%to_string(), "1110101001001001", &
                          "extract(10, 25) for '101101101010001110100011101010010010010010101110' "// &
                          "should return '1110101001001001'", &
                          stat, output_message=msg)

        subset = bitset%extract(31, 47)
        call expect_equal(subset%to_string(), "10110110101000111", &
                          "extract(31, 47) for '101101101010001110100011101010010010010010101110' "// &
                          "should return '10110110101000111'", &
                          stat, output_message=msg)

        subset = bitset%extract(16, 39)
        call expect_equal(subset%to_string(), "101000111010001110101001", &
                          "extract(16, 39) for '101101101010001110100011101010010010010010101110' "// &
                          "should return '101000111010001110101001'", &
                          stat, output_message=msg)

        ! subset = bitset%extract(40, 48)
        ! call expect_equal(subset%to_string(), "10110110", &
        !                   "extract(40, 48) for '101101101010001110100011101010010010010010101110' "// &
        !                   "should return '10110110'", &
        !                   stat, output_message=msg)
    end subroutine extract_returns_specified_range_of_bitset
end module test_operableBitset_unitTests_getValue
